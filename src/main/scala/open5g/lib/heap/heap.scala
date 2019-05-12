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

case class HeapConfig(  MemDeep    : Int,
                        ItemCfg    : HeapItemConfig,
                        insertFifo : Int
                        ) {
  def hasFifo = (insertFifo > 0)
  def AWidth = log2Up(MemDeep)
  def BA(x:UInt) : BAddress = {
    val r = BAddress(this)
    val xx = UInt(AWidth bits)
    xx := x.resize(AWidth)
    r.address := xx(AWidth-1 downto 1)
    r.lr := x(0)
    r
  }
  def cmp(a:UInt,b:UInt) : Bool = {
    val r = UInt(ItemCfg.KeyWidth bits)
    r := b - a
    !r(ItemCfg.KeyWidth-1)
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
    r
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

    val rl    = out Bool
    val dl    = out Bool
    val dr    = out Bool
    val dd    = out Bool
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
    data    = io.wd)
  
  HLeft.write(
    address = io.wa.address,
    enable  = !io.wa.lr && io.wen,
    data    = io.wd)
  
  left      := HLeft.readAsync(address = io.ra.address)
  right     := HRight.readAsync(address = io.ra.address)

  io.rl     := (right   <= left)
  dl        := (io.data <= left)
  dr        := (io.data <= right)
  io.dd     := Mux(io.ra.lr,dr,dl)
  io.dl     := dl
  io.dr     := dr 
  io.rd     := Mux(io.ra.lr,right,left)
  io.left   := left
  io.right  := right
}

object HeapState extends SpinalEnum {
  val sIdle, sCheck, sCheckDone, sPreUp, sUp, sUpDone, sPreDown, sDown, sDownDone, sWriteLast = newElement()
  defaultEncoding = SpinalEnumEncoding("staticEncoding")(
    sIdle       -> 0,
    sCheck      -> 1,
    sCheckDone  -> 2,
    sPreUp      -> 3,
    sUp         -> 4,
    sUpDone     -> 5,
    sPreDown    -> 6,
    sDown       -> 7,
    sDownDone   -> 8,
    sWriteLast  -> 9
    )
}

case class heap(cfg:HeapConfig,_debug:Boolean = false) extends Component {
  val io = new Bundle {
    val clear  = in  Bool
    val busy   = out Bool
    val size   = out UInt(cfg.AWidth bits)
    val output = master Stream(HeapItem(cfg))
    val insert = slave Stream(HeapItem(cfg))
    val now    = in UInt(cfg.ItemCfg.KeyWidth bits)
    val state  = out Bits(4 bits)
  }

  val size    = Reg(UInt(cfg.AWidth bits)) init(0)
  val data    = Reg(HeapItem(cfg)) 
  val state   = Reg(HeapState()) 
  val upReady = RegInit(False)
  val oValid  = RegInit(False)
  val output  = Reg(HeapItem(cfg)) 
  val addr    = Reg(BAddress(cfg)) 
  val now     = Reg(UInt(cfg.ItemCfg.KeyWidth bits)) init(0)

  val ra      = BAddress(cfg)
  val wa      = BAddress(cfg)
  val wen     = Bool
  val wd      = HeapItem(cfg)
  val rd      = HeapItem(cfg)

  val hm      = HeapMem(cfg)

  val insert  = Stream(HeapItem(cfg))

  if(cfg.hasFifo) {
    val fifo = StreamFifo( dataType = HeapItem(cfg), depth = cfg.insertFifo)
    fifo.io.push << io.insert
    fifo.io.pop >> insert
  } else {
    insert << io.insert
  }
  
  io.output.valid := oValid
  insert.ready := upReady
  io.state := state.asBits
  io.size := size
  io.output.payload := output
  
  now             := io.now

  hm.io.wd        := wd
  hm.io.ra        := ra
  hm.io.wa        := wa
  hm.io.wen       := wen
  hm.io.data      := data
  rd              := hm.io.rd

  import HeapState._
  /* read address logic */
  switch(state) {
    is(sCheck)    {ra := cfg.BA(U(1,cfg.AWidth bits))} 
    is(sPreDown)  {ra := cfg.BA(size)}
    is(sDown)     {ra := addr}
    is(sUp)       {ra := cfg.BA(addr.address)}
    default       {ra := cfg.BA(U(0,cfg.AWidth bits))}
  }
  /* write address logic */
  switch(state) {
    is(sDown) {wa := cfg.BA(addr.address)}
    is(sWriteLast) {wa := addr}
    is(sUp)   {wa := addr}
    default   {wa := cfg.BA(U(0,cfg.AWidth bits))}
  }
  /* write enable logic */
  switch(state) {
    is(sDown) {wen := True}
    is(sUp)   {wen := True}   
    default   {wen := False}
  }

  switch(state) {
    is(sIdle) {io.busy := False}
    default   {io.busy := True}
  }

  when(io.clear) {
    size := U(0)
    state := sIdle
    upReady := False
    wd.zero
  } otherwise {
    switch(state) {
      is(sIdle) {
        when(io.output.ready && size > 0) {
          state := sCheck
        }.elsewhen(insert.valid && size < cfg.MemDeep-2) {
          state := sPreUp
          upReady := True
        }
        wd.zero
      }
      is(sCheck) {
        when(cfg.cmp(rd.key,now)) {
          output := rd
          oValid := True
          state  := sCheckDone
        }.elsewhen(insert.valid && size < cfg.MemDeep-2) {
          state := sPreUp
          upReady := True
        }.elsewhen(!io.output.ready) {
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
        when(!hm.io.dl && !hm.io.rl && (ra.address * 2 < size)) {
          wd := hm.io.left
          when(ra.address.msb) {
            state := sWriteLast
            addr := cfg.BA(ra.address |<< 1)
          } otherwise {
            addr.address := ra.address |<< 1
          }
        }.elsewhen(!hm.io.dr && (ra.address * 2 + 1 < size)) {
          wd := hm.io.right
          when(ra.address.msb) {
            state := sWriteLast
            addr := cfg.BA((ra.address |<< 1) + 1)
          } otherwise {
            addr.address := (ra.address |<< 1) + 1
          }
        }.otherwise {
          wd := data
          state := sDownDone
        }
      }
      is(sWriteLast) {
        wd := data
        state := sDownDone
      }
      is(sDownDone) {
        state := sIdle
        wd.zero
      }
      is(sPreUp) {
        when(insert.fire) {
          data    := insert.payload
          upReady := False
          size    := size + 1
          addr    := cfg.BA(size + 1)
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
    }
  }
  
  val debug = if(_debug) new Bundle {
    val wa    = out UInt(cfg.AWidth bits)
    val ra    = out UInt(cfg.AWidth bits)
    val left  = out UInt(cfg.ItemCfg.KeyWidth bits)
    val right = out UInt(cfg.ItemCfg.KeyWidth bits)
    val data  = out UInt(cfg.ItemCfg.KeyWidth bits)
    val rd    = out UInt(cfg.ItemCfg.KeyWidth bits)
    val wd    = out UInt(cfg.ItemCfg.KeyWidth bits)
    val wen   = out Bool
  } else null

  if(_debug) {
    debug.wa    := wa.asAddr
    debug.ra    := ra.asAddr
    debug.left  := hm.io.left.key
    debug.right := hm.io.right.key
    debug.data  := data.key
    debug.rd    := hm.io.rd.key
    debug.wd    := wd.key
    debug.wen   := wen
  }
}

