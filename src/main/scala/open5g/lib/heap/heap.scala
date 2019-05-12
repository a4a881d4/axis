package open5g.lib.heap

import spinal.core._
import spinal.lib._


case class HeapItemConfig(  KeyWidth    : Int,
                            ValueWidth  : Int
                        )
case class HeapItem(cfg:HeapConfig) extends Bundle with IMasterSlave {
  val key = UInt(cfg.ItemCfg.KeyWidth bits)
  val value = UInt(cfg.ItemCfg.ValueWidth bits)
  def asMaster : Unit = {
    out(key,value)
  }
  def <=(that:HeapItem) : Bool = {
    cfg.cmp(this.key , that.key)
  }
  def zero : Unit = {
    key := U(0)
    value := U(0)
  }
}

case class HeapConfig(  MemDeep : Int,
                        ItemCfg : HeapItemConfig
                        ) {
  def AWidth = log2Up(MemDeep)
  def BA(x:UInt) : BAddress = {
    val r = BAddress(this)
    val xx = UInt(AWidth bits)
    xx := x.resize(AWidth)
    r.address := xx(AWidth-1 downto 1)
    r.lr := x(0)
    r
  }
  def cmp(a:UInt,b:UInt) : Bool {
    val r = b - a
    !r(cfg.ItemCfg.KeyWidth-1)
  }  
}

case class BAddress(cfg:HeapConfig) extends Bundle with IMasterSlave {
  val address = UInt(cfg.AWidth-1 bits)
  val lr      = Bool
  def asMaster = {
    out(address,lr)
  }
  def asAddr : UInt = {
    val r = UInt(cfg.AWidth bits)
    r(cfg.AWidth-1 downto 1) := address
    r(0) := lr
  }
}

case class HeapMem(cfg:HeapConfig) extends Component {
  val io = new Bundle {
    val ra    = slave(BAddress(cfg))
    val wa    = slave(BAddress(cfg))
    val wen   = in Bool

    val left  = master(HeapItem(cfg))
    val right = master(HeapItem(cfg))
    val rd    = master(HeapItem(cfg))

    val data  = slave(HeapItem(cfg))

    val wd    = slave(HeapItem(cfg))

    val lr    = Bool
    val dl    = Bool
    val dr    = Bool
    val dd    = Bool
  }
  val HRight = Mem(HeapItem(cfg),cfg.MemDeep/2)
  val HLeft  = Mem(HeapItem(cfg),cfg.MemDeep/2)
  val left   = HeapItem(cfg)
  val right  = HeapItem(cfg)
  val dl     = Bool
  val dr     = Bool

  HRight.write(
    address = io.wa.address,
    enable  = io.wa.lr && io.wen,
    data    = io.rd)
  
  HLeft.write(
    address = io.wa.address,
    enable  = !io.wa.lr && io.wen,
    data    = io.rd)
  
  left      := HLeft.read(address = io.ra.address)
  right     := HRight.read(address = io.ra.address)

  io.lr     := left <= right
  dl        := data <= left
  dr        := data <= right
  io.dd     := Mux(io.ra.lr,dr,dl)
  io.dl     := dl
  io.dr     := dr 
  io.rdata  := Mux(io.ra.lr,right,left)
  io.left   := left
  io.right  := right
}

object HeapState extends SpinalEnum {
  val sIdle, sCheck, sPreUp, sUp, sUpDone, sPreDown, sDown, sDownDone = newElement()
}

case class heap(cfg:HeapConfig) extends Component {
  val io = new Bundle {
    val clear  = in  Bool
    val udout  = out Bool
    val busy   = out Bool
    val size   = out UInt(cfg.AWidth bits)
    val output = master Stream(HeapItem(cfg))
    val insert = slave Stream(HeapItem(cfg))
    val now    = in UInt(cfg.ItemCfg.KeyWidth bits)
  }

  val size    = Reg(UInt(cfg.AWidth bits)) init(0)
  val data    = Reg(HeapItem(cfg))
  val state   = Reg(HeapState())
  val upReady = RegInit(False)
  val oValid  = RegInit(False)
  val output  = Reg(HeapItem(cfg))
  val addr    = Reg(BAddress(cfg))

  val ra      = BAddress(cfg)
  val wa      = BAddress(cfg)
  val wen     = Bool
  val wd      = HeapItem(cfg)

  val hm      = HeapMem(cfg)
  
  io.output.valid := oValid
  io.insert.ready := upReady

  hm.io.wd        := wd
  hm.io.ra        := ra
  hm.io.wa        := wa
  hm.io.wen       := wen
  hm.io.data      := data

  import HeapState._
  /* read address logic */
  switch(state) {
    is(sCheck)    ra := cfg.BA(U(1,cfg.AWidth bits)) 
    is(sPreDown)  ra := cfg.BA(size)
    is(sDown)     ra := addr
    is(sUp)       ra := cfg.BA(addr.address)
    default       ra := cfg.BA(U(0,cfg,AWidth bits))
  }
  /* write address logic */
  switch(state) {
    is(sDown) wa := cfg.BA(addr.address)
    is(sUp)   wa := addr
    default   wa := cfg.BA(U(0,cfg,AWidth bits))
  }
  /* write enable logic */
  switch(state) {
    is(sDown) wen := True
    is(sUp)   wen := True    
    default   wen := False
  }

  switch(state) {
    is(sIdle) io.busy := False
    default   io.busy := True
  }

  when(io.clear) {
    size := U(0)
    state := sIdle
    upReady := False
    wd.zeros
  } otherwise {
    switch(state) {
      is(sIdle) {
        when(io.output.ready && size > 0) {
          state := sCheck
        }.elsewhen(io.insert.valid && size < cfg.MemDeep-2) {
          state := sPreUp
          upReady := True
        }
        wd.zero
      }
      is(sCheck) {
        when(cfg.cmp(rd.key,io.now)) {
          output := rd
          oValid := True
          state  := sCheckDone
        }.elsewhen(io.insert.valid && size < cfg.MemDeep-2) {
          state := sPreUp
          upReady := True
        }.otherwise {
          state := sIdle
        }
        wd.zero
      }
      is(sCheckDone) {
        when(io.output.fire) {
          oValid := False
          state := sPreDown
        }
        wd.zero
      }
      is(sPreDown) {
        data := rd
        addr.address := U(1)
        state := sDown
        size := size - U(1)
        wd.zero
      }
      is(sDown) {
        val next   = UInt(cfg.AWidth bits)
        val sleft  = BAddress(cfg)
        val sright = BAddress(cfg)
        sleft.address := ra.address
        sleft.lr := False
        sright.address := ra.address
        sright.lr := True
        when(!hm.io.dl && (sleft.asAddr <= size)) {
          next := sleft.asAddr
          wd := hm.io.left
        }.elsewhen(!hm.io.dr && (sright.asAddr <= size)) {
          next := sright.asAddr
          wd := hm.io.right
        }.otherwise {
          wd := data
          state := sDownDone
        }
        addr.address := next(cfg.Awidth-2 downto 0)
      }
      is(sDownDone) {
        state := sIdle
        wd.zero
      }
      is(sPreUp) {
        when(io.insert.fire) {
          data    := io.insert.payload
          upReady := False
          size    := size + 1
          addr    := cfg.BA(U(size+1,cfg.AWidth bits))
          state   := sUp 
        }
        wd.zero
      }
      is(sUp) {
        addr := ra
        when(wa.asAddr === U(1)) {
          wd := data
          state := sUpDone
        }.elsewhen(hm.io.dd) {
          wd := hm.io.rd
        }.otherwise {
          wd := data
          state := sUpDone
        }
      }
      is(sUpDone) {
        state := sIdle
        wd.zero
      }
      default {
        wd.zero
      }
    }
  }
  val _debug = true
  val debug = if(_debug) new Bundle {
    val wa    = out UInt(cfg.AWidth bits)
    val ra    = out UInt(cfg.AWidth bits)
    val left  = out UInt(cfg.ItemCfg.KeyWidth bits)
    val right = out UInt(cfg.ItemCfg.KeyWidth bits)
    val data  = out UInt(cfg.ItemCfg.KeyWidth bits)
    val wen   = out Bool
  } else null

  if(_debug) {
    debug.wa    := wa.asAddr
    debug.ra    := ra.asAddr
    debug.left  := hm.io.left.key
    debug.right := hm.io.right.key
    debug.data  := hm.io.data.key
    debug.wen   := wen
  }
}

