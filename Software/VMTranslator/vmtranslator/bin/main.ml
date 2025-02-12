open Vmtranslator

let filepath = "/home/simeon/Desktop/vmtranslator/test.vm"

let () = Vmparser.read_file (String.trim filepath)
