MY_FSTAR_HOME=`cygpath -u $${FSTAR_HOME}`
MY_DKAL_HOME=`cygpath -d $${DKAL_HOME}`
FSTBIN=$(MY_DKAL_HOME)/ModularEngine/src/LogicEngine.FStar/bin
FSTBIN2=$(MY_DKAL_HOME)/ModularEngine/src/LogicEngine.FStar/bin2
DKAL_SIMPLEML=$(MY_DKAL_HOME)/ModularEngine/src/LogicEngine.SimpleML
FSTAR=fstar --UNSAFE_skip_target_checker --genIL --dotnet4 --odir bin --prims prims.fst
#typeHeaders.fst translationFromFStar.fst 
SOURCES=types.fst util.fst subst.fst typing.fst unify.fst coredkal2.fst net.fst crypto.fst marshall.fst state.fst interp.fst prog.fst clinical_trials.fst

interp: $(SOURCES)
	$(FSTAR) --writePrims $^ --o $@

coredkal2:
	fstar --prims prims.fst typeHeaders.fst types.fst translationFromFStar.fst util.fst subst.fst typing.fst unify.fst coredkal2.fst

coredkal2_genIL:
	fstar --prims prims.fst --writePrims typeHeaders.fst types.fst translationFromFStar.fst util.fst subst.fst typing.fst unify.fst coredkal2.fst --genIL --odir $(FSTBIN2) --UNSAFE

coredkal2_ok:
	# if commenting out the last few lines of types.fst
	# does not compile coredkal2.fst, does not raise anything
	fstar --UNSAFE_skip_target_checker --genIL --dotnet4 --odir $(FSTBIN2) --prims prims.fst typeHeaders.fst types.fst translationFromFStar.fst util.fst subst.fst typing.fst unify.fst

coredkal2_checker:
	#if commenting out the last few lines of types.fst
	# does not compile coredkal2.fst
	# raises an error on the target checker pass
	fstar --genIL --dotnet4 --odir $(FSTBIN2) --prims prims.fst typeHeaders.fst types.fst translationFromFStar.fst util.fst subst.fst typing.fst unify.fst

logicengine:
	$(FSTAR) --o LogicEngine.FStar typeHeaders.fst types.fst translationFromFStar.fst term.fst logicEngine.fst

coredkal:
	fstar --prims prims.fst typeHeaders.fst types.fst translationFromFStar.fst coredkal.fst # --genIL --odir $(FSTBIN)
	# fsc -r $(FSTBIN)/coredkal.dll coredkal_runtime.fs


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
