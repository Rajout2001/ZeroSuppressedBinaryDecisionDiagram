OCAMLC=ocamlc

EXECUTABLE= big_int decision_tree compressionListe compressionArbre distribution experimentales

COMPRESSION_DEP=zdd.cmi big_int.cmi decision_tree.cmi deja_vu.cmi dot.cmi 
COMPRESSION_ML=big_int.ml decision_tree.ml deja_vu.ml zdd.ml dot.ml 

all:test_big_int test_decision_tree test_compressionListe test_compressionArbre test_distribution test_experimentales
#----------------------------------------COMPILE---------------------------------------------

big_int.cmi big_int.cmo:
	$(OCAMLC) -c big_int.ml

decision_tree.cmi decision_tree.cmo: big_int.cmi
	$(OCAMLC) -c decision_tree.ml big_int.ml 

zdd.cmi zdd.cmo: big_int.cmi decision_tree.cmi deja_vu.cmi
	$(OCAMLC) -c zdd.ml big_int.ml decision_tree.ml

deja_vu.cmi deja_vu.cmo: big_int.cmo decision_tree.cmi
	$(OCAMLC) -c deja_vu.ml big_int.ml decision_tree.ml

dot.cmi dot.cmo: decision_tree.cmi big_int.cmi
	$(OCAMLC) -c dot.ml decision_tree.ml big_int.ml

distribution.cmi distribution.cmo: big_int.cmi decision_tree.cmi deja_vu.cmi zdd.cmi
	$(OCAMLC) -c distribution.ml big_int.ml decision_tree.ml deja_vu.ml zdd.ml

experimentales.cmi experimentales.cmo: big_int.cmi decision_tree.cmi deja_vu.cmi zdd.cmi
	$(OCAMLC) -c experimentales.ml big_int.ml decision_tree.ml deja_vu.ml zdd.ml


compile: big_int.cmi decision_tree.cmi deja_vu.cmi zdd.cmi dot.cmi distribution.cmi experimentales.cmi


#-----------------------------------------TEST-------------------------------------------------

test_big_int : big_int.cmi
	$(OCAMLC) -o big_int big_int.ml ./test/test_big_int.ml
	./big_int

test_decision_tree : dot.cmi decision_tree.cmi big_int.cmi
	$(OCAMLC) -o decision_tree big_int.ml decision_tree.ml dot.ml ./test/test_decision_tree.ml
	mkdir -p img
	./decision_tree
	dot -Tpng ./img/decision_tree.dot -o ./img/decision_tree.png

test_compressionListe: $(COMPRESSION_DEP)
	$(OCAMLC) -o compressionListe $(COMPRESSION_ML) ./test/test_compression_liste.ml 
	mkdir -p img
	./compressionListe
	dot -Tpng ./img/compressionParListe.dot -o ./img/compressionParListe.png

test_compressionArbre: $(COMPRESSION_DEP)
	$(OCAMLC) -o compressionArbre $(COMPRESSION_ML) ./test/test_compression_arbre.ml
	mkdir -p img
	./compressionArbre
	dot -Tpng ./img/compressionParArbre.dot -o ./img/compressionParArbre.png

test_distribution : distribution.cmi big_int.cmi decision_tree.cmi deja_vu.cmi zdd.ml
	$(OCAMLC) -o distribution big_int.ml decision_tree.ml deja_vu.ml zdd.ml distribution.ml
	./distribution

test_experimentales : experimentales.cmi big_int.cmi decision_tree.cmi deja_vu.cmi zdd.ml
	$(OCAMLC) -o experimentales big_int.ml decision_tree.ml deja_vu.ml zdd.ml experimentales.ml
	./experimentales
	

#-----------------------------------------CLEAN-----------------------------------------------

# Nettoyer les fichiers inutile
mr_proper:
	rm -f $(EXECUTABLE) *.cmo ./img/*.dot ./test/*.cmo

# Nettoyer les fichiers générés
clean:
	rm -f $(EXECUTABLE) *.cmo ./img/*.dot ./img/*.png *.cmi ./test/*.cmo ./test/*.cmi *.txt
