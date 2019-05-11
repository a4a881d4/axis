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
case class HeapItem(cfg:HeapItemConfig) extends Bundle with IMasterSlave {
  val key = UInt(cfg.KeyWidth bits)
  val value = UInt(cfg.ValueWidth bits)
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

case class heap(  cfg    : HeapConfig,
                  prefix : Constant) {
  val io = new Bundle {
    val insert = slave  Stream(HeapItem(cfg.ItemCfg))
    val output = master Stream(HeapItem(cfg.ItemCfg))
    val now    = UInt(cfg.ItemCfg.KeyWidth bits)
    val clear  = Bool
    val size   = UInt(cfg.AWidth bits)
  }
  val size  = Reg(UInt(cfg.AWidth bits)) init(0)
  val index = Reg(UInt(cfg.AWidth bits)) init(0)
  io.size := size - 1
  val raddr = UInt(cfg.AWidth bits)
  val ren   = Bool
  val ready = RegInit(False)

  output.ready := ready
  when(output.fire) {
    ready := False
  } otherwise {
    when(fsm.isActive(sDownReadAddr)) {

    }
  }

  val fsm = new StateMachine {
    val insertEnd := RegInit(False)
    val outputEnd := RegInit(False)
    val insertReady := RegInit(False)
    val sClear : State = new State with EntryPoint {
      whenIsActive {
        size := U(0)
        goto(sInsert)
      }
    }
    val sInsert : State = new State {
      whenIsActive {
        when(size < cfg.MemDeep-2 && insert.valid) {
          insertReady := True
          insertEnd   := False
          goto(sUpFirst)
        } otherwise {
          goto(sOutput)
        }
      }
    }
    val sUpFirst : State = new State {
      whenIsActive {
        size := size + 1
        insert.ready := True
        readAddr 
      }
    }
    val sOutput : State = new State {
      whenIsActive {
        when(io.clear) {
          goto(sClear)
        } otherwise {
          index := U(1)
          readen:= Ture
          goto(sDownReadAddr)
        }
      }
    }
}

case class HeapMem(cfg:HeapConfig) {
  val io = {
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
  }

  val HLeft  = Mem(HeapItem(cfg),cfg.MemDeep/2)
  val HRight = Mem(HeapItem(cfg),cfg.MemDeep/2)
  val ra     = UInt(cfg.AWidth bits)
  val wa     = UInt(cfg.AWidth bits)
  val wen    = Bool
  val left   = HeapItem(cfg)
  val right  = HeapItem(cfg)
  val wd     = HeapItem(cfg)

  ra  := Mux(ud,io.rau,io.rad)
  wa  := Mux(ud,io.wau,io.wad)
  wen := Mux(ud,io.wenu,io.wend)
  wd  := Mux(ud,io.wdu,io.wdd)

  io.left  := left
  io.right := right
  io.rout  := Mux(rao(0),right,left)

  val rao := Reg(UInt(cfg.AWidth bits)) init(0)
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
}

case class UP(cfg:HeapConfig) extends Component {
  val io = new Bundle {
    val insert = slave(HeapItem(cfg))
    val push   = in Bool
    val busy   = out Bool

    val ra  = out UInt(cfg.AWidth bits)
    val rar = in UInt(cfg.AWidth bits)
    val rd  = slave(HeapItem(cfg))

    val wa  = out UInt(cfg.AWidth bits)
    val wen = out Bool
    val wd  = master(HeapItem(cfg))
  }
}