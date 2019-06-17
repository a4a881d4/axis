package open5g.lib.virual

class MemIn(config:ringConfig) extends EndPoint(config) {
  val io = new Bundle {
    val alloc =  slave(Stream(Bits(config.phy bits)))
    val done  = master(Stream(memBlockRecord(config)))
    val w     = master(zcpsmIOW(config.blkAW+config.phy,config.Bwidth))
    val error = out Bool
  }
  when()
}