#readingskills data
library(party)
print(head(readingSkills))
input.dat<-readingSkills[c(1:105),]
png(file = "decision.png")
output.tree=ctree(nativeSpeaker ~ age + shoeSize + score, data = input.dat)
output.tree
plot(output.tree)
dev.off()

#life insurance data
library(party)
f=file.choose()
f1=read.csv(f)
input.dat<-f1[c(1:10),]
png(file = "decision1.png")
output.tree=ctree(Life.Ins.Promo ~ Age + Sex + CC.Ins + Income.Range , data = input.dat)
output.tree
plot(output.tree)
dev.off()
