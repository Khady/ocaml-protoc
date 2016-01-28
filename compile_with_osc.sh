FILES="
src/lib/backend_ocaml.ml
src/lib/backend_ocaml.mli
src/lib/backend_ocaml_static.ml
src/lib/encoding_util.ml
src/lib/encoding_util.mli
src/lib/exception.ml
src/lib/exception.mli
src/lib/fmt.ml
src/lib/fmt.mli
src/lib/graph.ml
src/lib/graph.mli
src/lib/lexer.mll
src/lib/logger.ml
src/lib/logger.mli
src/lib/ocaml_codegen.ml
src/lib/ocaml_codegen.mli
src/lib/ocaml_types.ml
src/lib/parser.mly
src/lib/pbpt.ml
src/lib/pbpt_util.ml
src/lib/pbpt_util.mli
src/lib/pbtt.ml
src/lib/pbtt_util.ml
src/lib/pbtt_util.mli
src/lib/util.ml
src/lib/util.mli
src/js-demo/demo.ml
"
rm -r js
mkdir -p js 

for file in $FILES
do 
    cp $file ./js
done

ocamllex js/lexer.mll
ocamlyacc js/parser.mly

FILES=`ls js/*.ml*`
FILES=`ocamldep -sort $FILES`

OSC_STDLIB=`which osc`
OSC_STDLIB=`dirname $OSC_STDLIB`/../stdlib

osc -I $OSC_STDLIB -I js $FILES 
