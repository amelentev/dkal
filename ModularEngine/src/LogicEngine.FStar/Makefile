MY_FSTAR_HOME=`cygpath -u $${FSTAR_HOME}`
MY_DKAL_HOME=`cygpath -d $${DKAL_HOME}`
FSTBIN=$(MY_DKAL_HOME)/ModularEngine/src/LogicEngine.FStar/bin
DKAL_SIMPLEML=$(MY_DKAL_HOME)/ModularEngine/src/LogicEngine.SimpleML
FSTAR=fstar --UNSAFE_skip_target_checker --genIL --dotnet4 --odir $(FSTBIN) --prims prims.fst

logicengine:
	$(FSTAR) --o LogicEngine.FStar typeHeaders.fst types.fst translationFromFStar.fst term.fst logicEngine.fst

coredkal:
	fstar --prims prims.fst typeHeaders.fst types.fst translationFromFStar.fst coredkal.fst # --genIL --odir $(FSTBIN)
	# fsc -r $(FSTBIN)/coredkal.dll coredkal_runtime.fs
	
clean:
	rm -f $(FSTBIN)/Types.dll $(FSTBIN)/Builders.dll $(FSTBIN)/Subst.dll $(FSTBIN)/TranslationtoFStar.dll $(FSTBIN)/TranslationfromFStar.dll $(FSTBIN)/Utilities.dll $(FSTBIN)/Term.dll $(FSTBIN)/FStarLogicEngineImpl.dll $(FSTBIN)/FStarLogicEngine.dll

prepare:
	rm -rf $(FSTBIN)
	mkdir -p $(FSTBIN)
	cp -r $(MY_FSTAR_HOME)/bin/*.dll $(FSTBIN)/
	$(FSTAR) --writePrims

