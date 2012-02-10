MY_FSTAR_HOME=`cygpath -u $${FSTAR_HOME}`
MY_DKAL_HOME=`cygpath -d $${DKAL_HOME}`
FSTBIN=$(MY_DKAL_HOME)/ModularEngine/src/LogicEngine.FStar/bin
FSTBIN2=$(MY_DKAL_HOME)/ModularEngine/src/LogicEngine.FStar/bin2
DKAL_SIMPLEML=$(MY_DKAL_HOME)/ModularEngine/src/LogicEngine.SimpleML
FSTAR=fstar --writePrims --UNSAFE --genIL --dotnet4 --odir bin --prims prims.fst --z3timeout 1000000
#typeHeaders.fst translationFromFStar.fst 
SOURCES=util.fst types.fst subst.fst typing.fst unify.fst hilbertian.fst infonlogic.fst crypto.fst authenticate.fst marshall.fst net.fst state.fst interp.fst 
SOURCES_INFONLOGIC=util.fst types.fst subst.fst typing.fst unify.fst hilbertian.fst infonlogic.fst 

retail: bin/retailcustomer.exe bin/retailwebsite.exe bin/retailpaypal.exe

calc: bin/alice.exe bin/bob.exe

clinical: bin/clinicalorg1.exe bin/clinicalsite1.exe bin/clinicalphys1.exe bin/clinicalkeymgr.exe
bin/%.exe: $(SOURCES) examples/%.fst
	$(FSTAR) --writePrims $^ --odir bin 

bin/interp.dll: bin/FSCrypto.dll $(SOURCES)
	$(FSTAR) --writePrims $(SOURCES) --odir bin 

bin/FSCrypto.dll: crypto.fs
	fsc -a -o bin/FSCrypto.dll crypto.fs

qil: util.fst types.fst subst.fst typing.fst unify.fst hilbertian.fst qil.fst testqil.fst
	$(FSTAR) $^ 

bin/parseDkal.exe:  parseDkal.fs
	fsc  --warn:0 -g -O --mlcompatibility  -o $@ -r FSharp.PowerPack.dll -r FSharp.PowerPack.Compatibility.dll -r bin/FParsecCS.dll -r bin/FParsec.dll parseDkal.fs

examples/%.fst: bin/parseDkal.exe examples/%.dkal
	bin/parseDkal.exe examples/$*.dkal > examples/$*.fst

wc: $(SOURCES)
	wc -l prims.fst $^

clean:
	rm -f $(FSTBIN)/Types.dll $(FSTBIN)/Builders.dll $(FSTBIN)/Subst.dll $(FSTBIN)/TranslationtoFStar.dll $(FSTBIN)/TranslationfromFStar.dll $(FSTBIN)/Utilities.dll $(FSTBIN)/Term.dll $(FSTBIN)/FStarLogicEngineImpl.dll $(FSTBIN)/FStarLogicEngine.dll

infonlogic:
	fstar --writePrims --UNSAFE_skip_target_checker --genIL --dotnet4 --odir bin --prims prims.fst --z3timeout 1000000 $(SOURCES_INFONLOGIC) testinfonlogic.fst

prepare:
	mkdir -p $(FSTBIN)
	cp -r $(MY_FSTAR_HOME)/bin/*.dll $(FSTBIN)/
	$(FSTAR) --writePrims

run:
	DkalMulti.exe ../../samples/MultiMain/clinical_trials.mdkal 1000 1000 -FStarLogicEngine

test:
	fsc -g -O --warn:0 -o $(FSTBIN)/test.exe -r FSharp.PowerPack.dll -r FSharp.PowerPack.Compatibility.dll -r bin/runtime.dll -r bin/Prims.dll -r bin/ProofLib.dll -r bin/Crypto.dll -r bin/Authenticate.dll -r bin/Net.dll -r bin/crypto.dll -r bin/interp.dll test.fs
