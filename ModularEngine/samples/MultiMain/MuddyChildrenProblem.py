import sys
import getopt

HEADER_DEFINITIONS= (""
"/// This example models the muddy children problem\n"
"///\n"
"type String = System.String\n"
"type Int = System.Int32\n"
"type Principal = Dkal.Principal\n"
"\n"
"relation dontKnow(P: Principal)\n"
"relation isMuddy(P: Principal)\n"
"relation isClean(P: Principal)\n"
"relation log(S: String)\n"
"relation question(N: Int)\n"
"relation hasConclusion()\n"
"relation startQuestionRound()\n"
"relation answered(P: Principal)\n"
"relation start()\n"
"relation end()\n"
"relation partialanswer()\n"
"\n"
"/// In the beginning, each child knows:\n"
"/// - every other child status\n"
"/// - that is ignorant of its own status\n"
"/// - that no round has elapsed\n"
"\n")

MOM_POLICY_TEMPLATE = ("---mom-----------------------------\n"
"knows start()\n"
"knows question(0)\n"
"\n"
"with X:Int\n"
"\tif start() && question(X) do\n"
"<FORGET_CHILDREN_STATUS_TEMPLATE>"
"<SEND_QUESTIONX_TEMPLATE>"
"\t\tforget start()\n"
"\n"
"with P: Principal\n"
"\tupon P said isMuddy(P) do\n"
"\t\tlearn isMuddy(P) learn answered(P)\n"
"\tupon P said isClean(P) do\n"
"\t\tlearn isClean(P) learn answered(P)\n"
"\tupon P said dontKnow(P) do\n"
"\t\tlearn dontKnow(P)\n"
"\t\tlearn answered(P)\n"
"\t\tlearn partialanswer()\n"
"\n"
"with X:Int, Y:Int\n"
"if\n"
"<CHILDREN_ANSWERED_TEMPLATE>"
"\tpartialanswer() &&\n"
"\tquestion(X) && asInfon({|\"basic\"|Y:=X+1|})\n"
"do\n"
"\tlearn start()\n"
"<FORGET_ANSWERS_TEMPLATE>"
"\tforget partialanswer()\n"
"\tforget question(X)\n"
"\tlearn question(Y)\n" )

CHILD_POLICY_TEMPLATE= ("---<CHILD_NAME>-----------------------------\n"
"knows dontKnow(me)\n"
"knows not end()\n"
"<NOONE_BUT_ME_SAID_ANYTHING_YET>\n"
"<EVERY_OTHER_CHILD_STATUS>\n"
"<LEARN_WHAT_PEOPLE_SAY>\n"
"with X:Int\n"
"\tupon mom said question(X) do\n"
"\t\tlearn question(X)\n"
"\t\tlearn startQuestionRound()\n"
"\n"
"<CHILD_DEDUCTION>\n"
"with X:Int\n"
"\tif question(X) do forget question(X)\n"
"if hasConclusion() do\n"
"\tforget hasConclusion()\n"
"\n"
"if hasConclusion() && isMuddy(me) && not end()\n"
"do\n"
"<SEND_MUDDY_TO_EVERYONE>"
"\trelearn end()\n"
"\n"
"if hasConclusion() && isClean(me) && not end()\n"
"do\n"
"<SEND_CLEAN_TO_EVERYONE>"
"\trelearn end()\n"
"\n"
"if hasConclusion() && dontKnow(me) && not end()\n"
"do\n"
"<SEND_DONT_KNOW_TO_EVERYONE>\n")

childrenNames= ["alice", "bob", "charlie", "dick", "emily", "frank", "gertie", "homer", "ione", "jack", "kevin", "leslie", "mary", "nathan", "ozzie",
				"peter", "quisani", "rosie", "steve", "trixie", "ulysses", "valerie", "walter", "xavier", "yuri", "zack"]

def createDeductionSteps(childIndex, children, muddy):
	result= ""
	# if someone already said he is clean and I don't know anything, I'm muddy. shouldn't happen anyway
	result= result + "with P: Dkal.Principal\n"
	result= result + "\tif P said isClean(P) && not end() && startQuestionRound() do\n"
	result= result + "\t\tlearn hasConclusion()\n"
	result= result + "\t\tlearn isMuddy(me)\n"
	result= result + "\t\tforget dontKnow(me)\n"
	result= result + "\t\tforget startQuestionRound()\n\n"

	# 1) more realistic behaviour of the child, model every single round.
	#		- If I don't see muddy faces, I'm muddy (0)
	#		- If at round X I see less than X muddy faces something went wrong :) So I don't do anything and it should hang if such thing happens
	#		- If at round X I see X+1 or more muddy children, I can't conclude anything (1)
	#	 	- If at round X I see exactly X muddy children, and still no one said anything, I must be muddy (2)
	#		- If at round X I see exactly X muddy children and already someone said she's muddy, I must be clean (3)
	#	 I calculate how many muddy faces I see from outside info (but this is not cheating, it is the same as counting muddy faces)
	muddyFacesISee= 0
	if childIndex < muddy:		# I am muddy (I don't know that anyway), so I see muddy-1 faces
		muddyFacesISee= muddy - 1
	else:						# else I see them all
		muddyFacesISee= muddy
	
	#(0)
	noMuddy= ""
	if muddyFacesISee == 0:
		noMuddy= "true"
	else:
		noMuddy= "false"
	result= result + "// I check whether I see no muddy faces (if I see no muddy faces, I must be muddy myself).\n"
	result= result + "if question(0) && asInfon({|\"basic\"|"+ noMuddy +"|}) && not end() do\n"
	result= result + "\tlearn hasConclusion()\n"
	result= result + "\tlearn isMuddy(me)\n"
	result= result + "\tforget dontKnow(me)\n"
	result= result + "\tforget startQuestionRound()\n\n"

	#(1)
	result= result + "// If I see a certain number of muddy faces and mom has asked less times than that, then I cannot be sure if I am muddy or not.\n"
	if muddyFacesISee != 0:
		result= result + "with X: Int\n"
		result= result + "\tif question(X) && asInfon({|\"basic\"|X < " + str(muddy) + "|}) && not end() && startQuestionRound() do\n"
		result= result + "\t\tlearn hasConclusion()\n"
		result= result + "\t\tforget startQuestionRound()\n\n"
	
	withNmuddyPrincipals= "with X:Int"
	distinctCondition= "asInfon({|\"basic\"|true|})"
	for i in range(0,muddyFacesISee):
		withNmuddyPrincipals= withNmuddyPrincipals + ", P" + str(i) + ": Principal"
		for j in range(0,i):
			distinctCondition= distinctCondition + " && asInfon({|\"basic\"|P" + str(i) + "!=P" + str(j) + "|})"
	if muddyFacesISee <= 1:
		distinctCondition= "asInfon({|\"basic\"|true|})"

	areMuddyAndDidntSay= ""
	for i in range(0,muddyFacesISee):
		areMuddyAndDidntSay= areMuddyAndDidntSay + "&& isMuddy(P" + str(i) + ") && not P" + str(i) + " said isMuddy(P" + str(i) + ") " 

	
	#(2)
	result= result + "// If mom has asked as many times as muddy faces I see, I need to pay attention to what the muddy children do.\n"
	result= result + "// If they don't say they are muddy, it is because they are not sure. If they are not sure it must be because they see another muddy face -- mine.\n"
	if muddyFacesISee != 0:
		result= result + withNmuddyPrincipals + "\n"
		result= result + "\tif question(X) && asInfon({|\"basic\"|X := " + str(muddyFacesISee) + "|}) && not end() && startQuestionRound() && " + distinctCondition + " " + areMuddyAndDidntSay + " do\n"
		result= result + "\t\tlearn hasConclusion()\n"
		result= result + "\t\tlearn isMuddy(me)\n"
		result= result + " \t\tforget dontKnow(me)\n" 
		result= result + " \t\tforget startQuestionRound()\n\n"
	
	someoneSaidIsMuddy= "(asInfon({|\"basic\"|false|})"
	for i in range(0,muddyFacesISee):
		someoneSaidIsMuddy= someoneSaidIsMuddy + "|| P" + str(i) + " said isMuddy(P" + str(i) + ")"
	someoneSaidIsMuddy= someoneSaidIsMuddy + ")"
	#(3)
	result= result + "// If however they do say they are muddy, they must be sure of that. Therefore they don't see another muddy face. I must be clean.\n"
	if muddyFacesISee != 0:
		result= result + withNmuddyPrincipals + "\n"
		result= result + "\tif question(X) && asInfon({|\"basic\"|X := " + str(muddyFacesISee) + "|}) && not end() && startQuestionRound() && " + distinctCondition + " && " + someoneSaidIsMuddy + " do\n"
		result= result + "\t\tlearn hasConclusion()\n"
		result= result + "\t\tlearn isClean(me)\n"
		result= result + "\t\tforget dontKnow(me)\n"
		result= result + "\t\tforget startQuestionRound()\n\n"
	
	return result
	
def createChildPolicy(childIndex, children, muddy):
	result= CHILD_POLICY_TEMPLATE.replace("<CHILD_NAME>", childrenNames[childIndex])
	noOneSaid= ""
	for i in range(0,children):
		if i != childIndex:
			noOneSaid= noOneSaid + "knows not " + childrenNames[i] + " said isMuddy(" + childrenNames[i] + ")\n"
			noOneSaid= noOneSaid + "knows not " + childrenNames[i] + " said isClean(" + childrenNames[i] + ")\n"
	result= result.replace("<NOONE_BUT_ME_SAID_ANYTHING_YET>", noOneSaid)
	
	otherStatus = ""
	for i in range(0, children):
		if i != childIndex and i < muddy:
			otherStatus= otherStatus + "knows isMuddy(" + childrenNames[i] + ")\n"
		elif i != childIndex:
			otherStatus= otherStatus + "knows isClean(" + childrenNames[i] + ")\n"
	result= result.replace("<EVERY_OTHER_CHILD_STATUS>", otherStatus)

	learnSay= ""
	for i in range(0, children):
		if i != childIndex:
			learnSay= learnSay + "upon " + childrenNames[i] + " said isMuddy(" + childrenNames[i] + ") do\n"
			learnSay= learnSay + "\trelearn " + childrenNames[i] + " said isMuddy(" + childrenNames[i] + ")\n"
			learnSay= learnSay + "upon " + childrenNames[i] + " said isClean(" + childrenNames[i] + ") do\n"
			learnSay= learnSay + "\trelearn " + childrenNames[i] + " said isClean(" + childrenNames[i] + ")\n"
	result= result.replace("<LEARN_WHAT_PEOPLE_SAY>", learnSay)
			
	sendMuddy= "\tsend to mom: isMuddy(me)\n"
	for i in range(0, children):
		if i != childIndex:
			sendMuddy= sendMuddy + "\tsend to " + childrenNames[i] + ": isMuddy(me)\n"
	result= result.replace("<SEND_MUDDY_TO_EVERYONE>", sendMuddy)
	
	sendClean= "\tsend to mom: isClean(me)\n"
	for i in range(0, children):
		if i != childIndex:
			sendClean= sendClean + "\tsend to " + childrenNames[i] + ": isClean(me)\n"
	result= result.replace("<SEND_CLEAN_TO_EVERYONE>", sendClean)
	
	sendDK= "\tsend to mom: dontKnow(me)\n"
	for i in range(0, children):
		if i != childIndex:
			sendDK= sendDK + "\tsend to " + childrenNames[i] + ": dontKnow(me)\n"
	result= result.replace("<SEND_DONT_KNOW_TO_EVERYONE>", sendDK)
	
	deductionSteps= createDeductionSteps(childIndex, children, muddy)
	result= result.replace("<CHILD_DEDUCTION>", deductionSteps)
	return result
			
def createMomPolicy(children, muddy):
	forgetStatus= "" 
	for i in range(0,children):
		forgetStatus= forgetStatus + "\t\tforget isMuddy(" + childrenNames[i] + ")\n"
		forgetStatus= forgetStatus + "\t\tforget isClean(" + childrenNames[i] + ")\n"
		forgetStatus= forgetStatus + "\t\tforget dontKnow(" + childrenNames[i] + ")\n"
	
	questions= ""
	for i in range(0, children):
		questions = questions + "\t\tsend to " + childrenNames[i] + ": question(X)\n"
	
	answers= ""
	for i in range(0, children):
		answers= answers + "\tanswered(" + childrenNames[i] + ") && \n"
	
	forgetAnswers= ""
	for i in range(0, children):
		forgetAnswers= forgetAnswers + "\tforget answered(" + childrenNames[i] + ")\n"

	result= MOM_POLICY_TEMPLATE.replace("<FORGET_CHILDREN_STATUS_TEMPLATE>", forgetStatus)
	result= result.replace("<SEND_QUESTIONX_TEMPLATE>", questions)
	result= result.replace("<CHILDREN_ANSWERED_TEMPLATE>", answers)
	result= result.replace("<FORGET_ANSWERS_TEMPLATE>", forgetAnswers)
	return result

def showUsage():
	print("MuddyChildrenProblem -- builds an instance of the muddy children problem for DKAL to solve. Children are named by different initials, so no more than 26 are allowed.")
	print("Usage:")
	print("\tMuddyChildrenProblem --children <number_of_children> --muddy <number_of_muddy_children>")
	print("Options:")
	print("\t--children <number_of_children>: total children in the group, at least 2 and at most 26. Short form: -c. Required.")
	print("\t--muddy <number_of_muddy_children>: Number of muddy children, between 1 and number_of_children. Short form: -m. Required.")

def main():
	try:
		opts, args= getopt.getopt(sys.argv[1:], "c:m:", ["children=","muddy="])
	except getopt.error as msg:
		print(msg)
		showUsage()
		sys.exit(2)

	if len(opts) < 2:
		print("Missing options")
		showUsage()
		sys.exit(2)

	children = muddy = 0
	for o, a in opts:
		if o in ["-c","--children"]:
			children= int(a)
		if o in ["-m", "--muddy"]:
			muddy= int(a)

	if children < 2 or children > 26:
		print("At least 2 children are necessary, and no more than 26")
		showUsage()
		sys.exit(2)

	if muddy < 1:
		print("Muddy children must be between 1 and the total children")
		showUsage()
		sys.exit(2)
   
	if children < muddy:
		print("Muddy children must be between 1 and the total children")
		showUsage()
		sys.exit(2)

	dkalPolicyFile= open("muddychildren_" + str(children) + "_" + str(muddy) + ".mdkal", "w")
	dkalPolicyFile.write(HEADER_DEFINITIONS)
	dkalPolicyFile.write(createMomPolicy(children, muddy) + "\n")
	for i in range(0,children):
		dkalPolicyFile.write(createChildPolicy(i, children, muddy) + "\n")
	dkalPolicyFile.close()
	sys.exit(0)

if __name__ == "__main__":
	main()
 