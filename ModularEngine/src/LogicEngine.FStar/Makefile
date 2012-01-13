MY_FSTAR_HOME=`cygpath -u $${FSTAR_HOME}`
MY_DKAL_HOME=`cygpath -d $${DKAL_HOME}`
FSTBIN=$(MY_DKAL_HOME)/ModularEngine/src/LogicEngine.FStar/bin
FSTBIN2=$(MY_DKAL_HOME)/ModularEngine/src/LogicEngine.FStar/bin2
DKAL_SIMPLEML=$(MY_DKAL_HOME)/ModularEngine/src/LogicEngine.SimpleML
FSTAR=fstar --writePrims --UNSAFE_skip_target_checker --genIL --dotnet4 --odir bin --prims prims.fst --z3timeout 1000000
#typeHeaders.fst translationFromFStar.fst 
SOURCES=util.fst types.fst subst.fst typing.fst unify.fst infonlogic.fst crypto.fst net.fst keys.fst state.fst marshall.fst authenticate.fst interp.fst prog.fst clinical_trials.fst

interp: $(SOURCES)
	$(FSTAR) --writePrims $^ --o $@

qil: util.fst types.fst subst.fst typing.fst unify.fst hilbertian.fst qil.fst testqil.fst
	$(FSTAR) $^ 

wc: $(SOURCES)
	wc -l prims.fst $^

clean:
	rm -f $(FSTBIN)/Types.dll $(FSTBIN)/Builders.dll $(FSTBIN)/Subst.dll $(FSTBIN)/TranslationtoFStar.dll $(FSTBIN)/TranslationfromFStar.dll $(FSTBIN)/Utilities.dll $(FSTBIN)/Term.dll $(FSTBIN)/FStarLogicEngineImpl.dll $(FSTBIN)/FStarLogicEngine.dll

prepare:
	rm -rf $(FSTBIN)
	mkdir -p $(FSTBIN)
	cp -r $(MY_FSTAR_HOME)/bin/*.dll $(FSTBIN)/
	$(FSTAR) --writePrims

run:
	DkalMulti.exe ../../samples/MultiMain/clinical_trials.mdkal 1000 1000 -FStarLogicEngine
