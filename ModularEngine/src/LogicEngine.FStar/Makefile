MY_FSTAR_HOME=`cygpath -u $${FSTAR_HOME}`
MY_DKAL_HOME=`cygpath -d $${DKAL_HOME}`
FSTBIN=$(MY_DKAL_HOME)/ModularEngine/src/LogicEngine.FStar/bin
FSTBIN2=$(MY_DKAL_HOME)/ModularEngine/src/LogicEngine.FStar/bin2
DKAL_SIMPLEML=$(MY_DKAL_HOME)/ModularEngine/src/LogicEngine.SimpleML
FSTAR=fstar --UNSAFE_skip_target_checker --genIL --dotnet4 --odir $(FSTBIN) --prims prims.fst

coredkal2:
	# currently raises the logic_function bug
	fstar --prims prims.fst typeHeaders.fst types.fst translationFromFStar.fst util.fst subst.fst typing.fst unify.fst coredkal2.fst --genIL --odir $(FSTBIN2)

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

wc: 
	wc -l prims.fst typeHeaders.fst types.fst translationFromFStar.fst util.fst subst.fst typing.fst unify.fst coredkal2.fst 

clean:
	rm -f $(FSTBIN)/Types.dll $(FSTBIN)/Builders.dll $(FSTBIN)/Subst.dll $(FSTBIN)/TranslationtoFStar.dll $(FSTBIN)/TranslationfromFStar.dll $(FSTBIN)/Utilities.dll $(FSTBIN)/Term.dll $(FSTBIN)/FStarLogicEngineImpl.dll $(FSTBIN)/FStarLogicEngine.dll

prepare:
	rm -rf $(FSTBIN)
	mkdir -p $(FSTBIN)
	cp -r $(MY_FSTAR_HOME)/bin/*.dll $(FSTBIN)/
	$(FSTAR) --writePrims

