package open5g.lib.heap

import spinal.core._
import spinal.lib._

import open5g.lib.common.Constant

/***
  *
  PointerType _insert(PointerType a)
  {
    if(m_pa->size < Hsize::value) {
      USRT_SCOPED_LOCK(m_pa->heap_mutex);
      m_pa->heap[m_pa->size] = a;
      m_pa->size++;
      up(m_pa->size-1);
      return a;
    } else
      return PointerType::Null();
  };
  PointerType del(PointerType a)
  {
    PointerType ret = PointerType::Null();
    USRT_SCOPED_LOCK(m_pa->heap_mutex);
    if(m_pa->size == 0) 
      return ret;

    int i;
    for(i = 0;i < m_pa->size;i++) {
      if(a == m_pa->heap[i])
        break;
    }
    
    if(i != m_pa->size) {
      m_pa->size--;
      m_pa->heap[i]=m_pa->heap[m_pa->size];
      down(i);
      ret = a;
    }
    
    return ret;
  };
  PointerType pop() {
    return del(m_pa->heap[0]);
  };
  void down(size_t index)
  {
    if(m_pa->size <= index)
      return;
    
    size_t left = index*2+1;
    size_t right = index*2+2;
    size_t small = index;
    
    if((left < m_pa->size) 
      && less(m_pa->heap[left],m_pa->heap[small]))
        small=left;
    
    if((right < m_pa->size) 
      && less(m_pa->heap[right],m_pa->heap[small]))
        small=right;
    
    if(small == index)
      return;
    
    auto a = m_pa->heap[index];
    m_pa->heap[index] = m_pa->heap[small];
    m_pa->heap[small] = a;
    
    down(small);
  };
  void up(size_t index)
  {
    if(index == 0)
      return;

    size_t _up = (index-1)/2;
    if(less(m_pa->heap[index],m_pa->heap[_up])) {
      auto a = m_pa->heap[index];
      m_pa->heap[index] = m_pa->heap[_up];
      m_pa->heap[_up] = a;
      up(_up);
    }
  };
  *
***/


/**
  * up processor
  * size := size+1, waddr := size+1, data := insert, index := size+1
  * read (waddr >> 1) readout, 
  * if(data < readout) write [addr] <- readout, addr := index
  * elseif() write[index] <- data :end
**/

/**
  * down processor
  * size := size-1, read index*2, 
  * if(readout > )
  * read index*2+1

  */
case class HeapItemConfig(  KeyWidth    : Int,
                            ValueWidth  : Int
                        )
case class HeapItem(cfg:HeapConfig) extends Bundle with IMasterSlave {
  val key = UInt(cfg.ItemCfg.KeyWidth bits)
  val value = UInt(cfg.ItemCfg.ValueWidth bits)
  def asMaster : Unit = {
    out(key,value)
  }
  def <(that:HeapItem) : Bool = {
    this.key < that.key
  }
}

case class HeapConfig(  MemDeep : Int,
                        ItemCfg : HeapItemConfig
                        ) {
  def AWidth = log2Up(MemDeep)
}

case class heap(cfg : HeapConfig) extends Component {
  val io = new Bundle {
    val insert = slave  Stream(HeapItem(cfg))
//    val output = master Stream(HeapItem(cfg))
    val now    = in UInt(cfg.ItemCfg.KeyWidth bits)
    val clear  = in Bool
    val size   = out UInt(cfg.AWidth bits)
  }


  val up = UP(cfg)
  val hm = HeapMem(cfg,true)
  

  up.io.insert << io.insert
  
  hm.io.ud := True
  
  hm.io.rau := up.io.ra
  hm.io.wdu := up.io.wd
  hm.io.wau := up.io.wa
  hm.io.wenu := up.io.wen
  up.io.rao := hm.io.rao
  up.io.rd := hm.io.rout

  val size = Reg(UInt(cfg.AWidth bits)) init(0)

  when(io.clear) {
    size := U(0)
  }.elsewhen(up.io.busy) {
    size := up.io.sizeo
  }

  up.io.sizei := size
  up.io.upen := !io.clear
  io.size := size

  val _debug = true
  if(_debug) {
    val debug = new Bundle {
      val dra  = in  UInt(cfg.AWidth bits) 
      val ddo  = master(HeapItem(cfg)) 
    }
    hm.io.dra := debug.dra
    debug.ddo := hm.io.ddo

    /* down */
    hm.io.wdd := up.io.wd
    hm.io.wad := up.io.wa
    hm.io.wend := up.io.wen
    
    hm.io.rad := up.io.ra
  
  }
}

case class HeapMem(cfg:HeapConfig,debug:Boolean=false) extends Component {
  val io = new Bundle{
    val rout = master(HeapItem(cfg))
    val left = master(HeapItem(cfg))
    val right= master(HeapItem(cfg))
    
    val wdu  = slave(HeapItem(cfg))
    val wdd  = slave(HeapItem(cfg))
    
    val rau  = in  UInt(cfg.AWidth bits)
    val rad  = in  UInt(cfg.AWidth bits)

    val rao  = out UInt(cfg.AWidth bits)

    val wau  = in  UInt(cfg.AWidth bits)
    val wenu = in  Bool
    val wad  = in  UInt(cfg.AWidth bits)
    val wend = in  Bool

    val ud   = in  Bool

    val dra  = if(debug) in  UInt(cfg.AWidth bits) else null
    val ddo  = if(debug) master(HeapItem(cfg)) else null
  }

  val HLeft  = Mem(HeapItem(cfg),cfg.MemDeep/2)
  val HRight = Mem(HeapItem(cfg),cfg.MemDeep/2)
  val ra     = UInt(cfg.AWidth bits)
  val wa     = UInt(cfg.AWidth bits)
  val wen    = Bool
  val left   = HeapItem(cfg)
  val right  = HeapItem(cfg)
  val wd     = HeapItem(cfg)
  val rao    = Reg(UInt(cfg.AWidth bits)) init(0)
  
  ra  := Mux(io.ud,io.rau,io.rad)
  wa  := Mux(io.ud,io.wau,io.wad)
  wen := Mux(io.ud,io.wenu,io.wend)
  wd  := Mux(io.ud,io.wdu,io.wdd)

  io.left  := left
  io.right := right
  io.rout  := Mux(rao(0),right,left)

  rao     := ra
  io.rao  := rao

  HLeft.write(
    address = wa(wa.high downto 1),
    enable  = !wa(0) && wen,
    data    = wd)

  HRight.write(
    address = wa(wa.high downto 1),
    enable  = wa(0) && wen,
    data    = wd)

  left := HLeft.readSync(
    address = ra(ra.high downto 1)
    )
  right := HRight.readSync(
    address = ra(ra.high downto 1)
    )
  if(debug) {
    val da0 = Reg(Bool)
    da0 := io.dra(0)
    io.ddo := Mux(da0,HRight.readSync(address=io.dra(io.dra.high downto 1)),
      HLeft.readSync(address=io.dra(io.dra.high downto 1)))
  }
}

case class UP(cfg:HeapConfig) extends Component {
  val io = new Bundle {
  
    val insert = slave Stream(HeapItem(cfg))
    
    val ra     = out UInt(cfg.AWidth bits)
    val rao    = in UInt(cfg.AWidth bits)
    val rd     = slave(HeapItem(cfg))

    val wa     = out UInt(cfg.AWidth bits)
    val wen    = out Bool
    val wd     = master(HeapItem(cfg))

    val sizeo  = out UInt(cfg.AWidth bits)
    val sizei  = in  UInt(cfg.AWidth bits)
  
    val busy   = out Bool
    val upen   = in  Bool
  }
  
  val busy = RegInit(False)
  val ra = Reg(UInt(cfg.AWidth bits)) init(0)
  val wa = Reg(UInt(cfg.AWidth bits)) init(0)
  val lra = Reg(UInt(cfg.AWidth bits)) init(0)
  val wen = RegInit(False)
  val wd = Reg(HeapItem(cfg))
  val size = Reg(UInt(cfg.AWidth bits)) init(0)
  val data = Reg(HeapItem(cfg))
  val s1 = UInt(cfg.AWidth bits)
  val first = RegInit(False)

  s1 := size + U(1)
  io.busy := busy
  io.sizeo := size

  io.ra := ra
  io.wa := wa
  io.wen := wen
  io.wd := wd

  when(!busy) {
    size := io.sizei
  } otherwise {
    when(io.insert.fire) {
      size := s1
    }
  }

  io.insert.ready := !busy && (size < cfg.MemDeep-2) && io.upen
  when(io.insert.fire) {
    busy := True
    data := io.insert.payload
    lra  := s1
    ra   := s1 >> U(1)
    wen  := False
    first:= True 
  }.elsewhen(busy) {
    ra := io.rao >> U(1) 
    when(first) {
      first := False
      wen := False
    } otherwise {
      wen := True
      wa  := lra
      lra := io.rao 
      when(data < io.rd) {
        wd := io.rd
      } otherwise {
        wd := data
      }
      when(data < io.rd || lra < 2) {
        busy := False
      }
    }
  }.otherwise {
    wen := False
  }
}

