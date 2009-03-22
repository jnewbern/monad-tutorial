default: MonadTutorial.pdf

PDFS =  TutorialOverview.pdf SectionOne.pdf SectionTwo.pdf SectionThree.pdf NewMonads.pdf InterpExercise.pdf

MonadTutorial.pdf: $(PDFS)
	pdfjoin $(PDFS)
	mv InterpExercise-joined.pdf MonadTutorial.pdf

test:
	acroread MonadTutorial.pdf
