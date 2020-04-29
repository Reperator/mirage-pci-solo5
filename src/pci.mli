include Mirage_pci.S

val connect :
  dma_offset:int -> dma_size:int -> string -> t Lwt.t
