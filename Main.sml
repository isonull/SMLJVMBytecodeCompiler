structure Test = struct

  open OS.Process

  exception InputError

  fun echo s = system ("echo \"" ^ s ^ "\"")
  fun readFile f = Frontend.readFile f
  fun cp f1 f2 = system ("yes | cp -rf " ^ f1 ^ " " ^ f2)
  fun mkdir f = system ("mkdir " ^ f)

  val basClassFiles = "./java-utils/*"

  fun main (prog_name, args) = let
    val progname = List.nth (args, 0)
    val infile   = List.nth (args, 1) 
    val outdir   = List.nth (args, 2) 
    val repfile  = outdir ^ "/report.txt"
    val repstm   = TextIO.openOut repfile
    fun repout s = TextIO.outputSubstr 
      (repstm, Substring.extract (s, 0, NONE))
    val fst = Frontend.parse infile
    val cst = CoreSyntaxTree.fromProg fst
    val (env, ist) = StaticInference.inference cst 
    val (spa, imr, lab) = InterInference.inference ist
    val imropt = InterOptimise.opt imr lab
    val cfs = NewCodeGeneration.genProg imropt spa 
    val rep = CoreSyntaxTree.toString cst in
    TIO.println ("Program name: " ^ progname);
    TIO.println ("Input file: " ^ infile);
    TIO.println ("Output directory: " ^ outdir);
    mkdir outdir;
    cp basClassFiles (outdir ^ "\\");
    NewCodeGeneration.writeClsList cfs outdir;
    repout (CoreSyntaxTree.toString cst);
    repout (IntermediateSyntaxTree.toString ist);
    repout (InterProgram.toString imr);
    repout "----- INTERMEDIATE REPRESENTATION OPTIMISED -----";
    repout (InterProgram.toString imropt);
    repout (Environment.toString env);
    repout (Space.toString spa);
    TextIO.flushOut repstm;
    TIO.println "FINISH";
    1
  end handle _ => (TIO.println "ERROR"; 0)

end
