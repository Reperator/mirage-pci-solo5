open Lwt.Infix
open OS.Solo5

let src = Logs.Src.create "pci" ~doc:"Mirage Solo5 PCIe module"
module Log = (val Logs.src_log src : Logs.LOG)

type error = Mirage_pci.Pci.error

let pp_error = Mirage_pci.Pci.pp_error

type solo5_pci_info =
  { vendor_id : int
  ; device_id : int
  ; class_code : int
  ; subclass_code : int
  ; progif : int
  ; bus_master_enable : bool
  ; bar0_buffer : Cstruct.buffer
  ; bar1_buffer : Cstruct.buffer
  ; bar2_buffer : Cstruct.buffer
  ; bar3_buffer : Cstruct.buffer
  ; bar4_buffer : Cstruct.buffer
  ; bar5_buffer : Cstruct.buffer
  }

type t =
  { id : string
  ; mutable active : bool
  ; info : solo5_pci_info
  ; bar0 : Cstruct.t
  ; bar1 : Cstruct.t
  ; bar2 : Cstruct.t
  ; bar3 : Cstruct.t
  ; bar4 : Cstruct.t
  ; bar5 : Cstruct.t
  ; dma : Cstruct.t
  }

external solo5_pci_acquire : string -> solo5_result * solo5_pci_info =
  "mirage_solo5_pci_acquire"

external solo5_dma_acquire : unit -> solo5_result * Cstruct.buffer =
  "mirage_solo5_dma_acquire"

let solo5_dma =
  match solo5_dma_acquire () with
  | SOLO5_R_OK, buffer -> Cstruct.of_bigarray buffer
  | SOLO5_R_AGAIN, _ -> assert false
  | SOLO5_R_EINVAL, _ ->
    Log.warn (fun f -> f "No DMA memory allocated.");
    Cstruct.empty
  | SOLO5_R_EUNSPEC, _ ->
    failwith (Fmt.strf "Pci: solo5_dma: Unspecified error")

let connect ~dma_offset ~dma_size devname =
  match solo5_pci_acquire devname with
  | SOLO5_R_OK, info ->
    let wrap_buffer buffer =
      if Bigarray.Array1.size_in_bytes buffer <> 0 then
        Cstruct.of_bigarray buffer
      else
        Cstruct.empty in
    Lwt.return
      { id = devname
      ; active = true
      ; info
      ; bar0 = wrap_buffer info.bar0_buffer
      ; bar1 = wrap_buffer info.bar1_buffer
      ; bar2 = wrap_buffer info.bar2_buffer
      ; bar3 = wrap_buffer info.bar3_buffer
      ; bar4 = wrap_buffer info.bar4_buffer
      ; bar5 = wrap_buffer info.bar5_buffer
      ; dma = Cstruct.sub solo5_dma dma_offset dma_size
      }
  | SOLO5_R_AGAIN, _ -> assert false
  | SOLO5_R_EINVAL, _ ->
    Lwt.fail_with (Fmt.strf "Pci: connect(%s): Invalid argument" devname)
  | SOLO5_R_EUNSPEC, _ ->
    Lwt.fail_with (Fmt.strf "Pci: connect(%s): Unspecified error" devname)

let vendor_id t = t.info.vendor_id

let device_id t = t.info.device_id

let class_code t = t.info.class_code

let subclass_code t = t.info.subclass_code

let progif t = t.info.progif

let bar0 t = if Cstruct.len t.bar0 = 0 then None else Some t.bar0

let bar1 t = if Cstruct.len t.bar1 = 0 then None else Some t.bar1

let bar2 t = if Cstruct.len t.bar2 = 0 then None else Some t.bar2

let bar3 t = if Cstruct.len t.bar3 = 0 then None else Some t.bar3

let bar4 t = if Cstruct.len t.bar4 = 0 then None else Some t.bar4

let bar5 t = if Cstruct.len t.bar5 = 0 then None else Some t.bar5

let dma t = t.dma

let name t = t.id

let disconnect t = t.active <- false; Lwt.return_unit
