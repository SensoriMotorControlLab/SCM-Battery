library(dplyr)
library(tidyr)
library(lubridate)

#### create data ####

# q2 <- read.csv('data/2020/fall/questionnaire_2.csv', stringsAsFactors=F)

q_full_1 <- read.csv('data/Canbyork-Fall2020-Part+#1-FINAL_February+24,+2023_15.58.csv', stringsAsFactors=F)
q_full_2 <- read.csv('data/Canbyork-Fall2020-Part+#2-FINAL_February+24,+2023_15.58.csv', stringsAsFactors=F)

# reading questionnaire
q_ie_part1_fall <- read.csv('data/Immediate+Cannabis+Effects-+Fall21-Part+1_November+2,+2022_19.36.csv', stringsAsFactors=F)
q_ie_part2_fall <- read.csv('data/Immediate+Cannabis+Effects-+Fall21-Part+2_November+2,+2022_19.40.csv', stringsAsFactors=F)
q_ie_part1_winter <- read.csv('data/Immediate+Cannabis+Effects-+Winter22-Part+1_November+2,+2022_19.48.csv', stringsAsFactors=F)
q_ie_part2_winter <- read.csv('data/Immediate+Cannabis+Effects-+Winter22-Part+2_November+2,+2022_19.50.csv', stringsAsFactors=F)

# part 3 and 4
q_ie_part3_fall <- read.csv('data/Immediate+Cannabis+Effects-+Part+3_November+2,+2022_19.43.csv', stringsAsFactors=F)
q_ie_part4_fall <- read.csv('data/Immediate+Cannabis+Effects-+Part+4_November+2,+2022_19.46.csv', stringsAsFactors=F)
q_ie_part3_winter <- read.csv('data/Immediate+Cannabis+Effects-+Winter22-Part+3_November+2,+2022_19.53.csv', stringsAsFactors=F)
q_ie_part4_winter <- read.csv('data/Immediate+Cannabis+Effects-+Winter22-Part+4_November+2,+2022_19.56.csv', stringsAsFactors=F)


#### create codebooks ####

# list of data frames
#df_list_q <- list(q_2020_fall,
#                q_2020_summer,
#                q_ie_part1_fall,
#                q_ie_part2_fall,
#                q_ie_part1_winter,
#                q_ie_part2_winter,
#                q_ie_part3_fall,
#                q_ie_part4_fall,
#                q_ie_part3_winter,
#                q_ie_part4_winter,
#                q_full_1,
#                q_full_2)

#df_names <- c("q_2020_fall",
#             "q_2020_summer",
#             "q_ie_part1_fall",
#             "q_ie_part2_fall",
#             "q_ie_part1_winter",
#             "q_ie_part2_winter",
#             "q_ie_part3_fall",
#             "q_ie_part4_fall",
#             "q_ie_part3_winter",
#             "q_ie_part4_winter",
#             "q_full_1",
#             "q_full_2")

# iterate over data frames
#for (i in seq_along(df_list_q)) {
  # create data frame with column names, numbers, and values
#  col_info <- data.frame(
#    num = 1:ncol(df_list_q[[i]]),
#    name = names(df_list_q[[i]]),
#    values = sapply(df_list_q[[i]], function(x) paste(unique(x), collapse = ", "))
#  )
  
  # write to csv file
#  write.csv(col_info, file.path("data", "codebook", paste0(df_names[i], "_codebook.csv")), row.names = FALSE)
#}

#### change the dates ####

q_full_1$date <- as.POSIXct(q_full_1$End.Date, format = "%Y-%m-%d %H:%M")
q_full_2$date <- as.POSIXct(q_full_2$End.Date, format = "%Y-%m-%d %H:%M")

# ie part 1 and 2
q_ie_part1_fall$date <- as.POSIXct(q_ie_part1_fall$End.Date, format = "%Y-%m-%d %H:%M")
q_ie_part2_fall$date <- as.POSIXct(q_ie_part2_fall$End.Date, format = "%Y-%m-%d %H:%M")
q_ie_part1_winter$date <- as.POSIXct(q_ie_part1_winter$End.Date, format = "%Y-%m-%d %H:%M")
q_ie_part2_winter$date <- as.POSIXct(q_ie_part2_winter$End.Date, format = "%Y-%m-%d %H:%M")

# part 3 and 4
q_ie_part3_fall$date <- as.POSIXct(q_ie_part3_fall$End.Date, format = "%Y-%m-%d %H:%M")
q_ie_part4_fall$date <- as.POSIXct(q_ie_part4_fall$End.Date, format = "%Y-%m-%d %H:%M")
q_ie_part3_winter$date <- as.POSIXct(q_ie_part3_winter$End.Date, format = "%Y-%m-%d %H:%M")
q_ie_part4_winter$date <- as.POSIXct(q_ie_part4_winter$End.Date, format = "%Y-%m-%d %H:%M")



q_full_1$date_date <- as.Date(q_full_1$date)
q_full_2$date_date <- as.Date(q_full_2$date)


q_ie_part1_fall$date_date <- as.Date(q_ie_part1_fall$date)
q_ie_part2_fall$date_date <- as.Date(q_ie_part2_fall$date)
q_ie_part1_winter$date_date <- as.Date(q_ie_part1_winter$date)
q_ie_part2_winter$date_date <- as.Date(q_ie_part2_winter$date)

# part 3 and 4
q_ie_part3_fall$date_date <- as.Date(q_ie_part3_fall$date)
q_ie_part4_fall$date_date <- as.Date(q_ie_part4_fall$date)
q_ie_part3_winter$date_date <- as.Date(q_ie_part3_winter$date)
q_ie_part4_winter$date_date <- as.Date(q_ie_part4_winter$date)

#### clean each of the questionnaires dataframes ####

#### full part 1 ####

#q_full_1

columns <- c("id",
             "date_date",
             "Finished",
             "Informed.Consent.Form...Study.Name..Visuomotor.learning....Researchers..Dr..Denise.Henriques....Purpose.of.the.Research...Our.research.team.is.interested.in.how.people.perceive..and.remember.the.location.and.characteristics.of.objects..and.how.they.move.a.mouse.cursor.to.visual.targets.under.various.circumstances.....What.You.Will.Be.Asked.to.Do.in.the.Research..You.will.be.asked.to.sit.in.front.of.a.computer.to.perceive..remember..discriminate.between.objects..and.or.acquire.targets..displayed.on.a.computer.monitor...These.short.tasks.can.include..1..detecting.objects.among.other.objects.or.changes.in.these.objects.after.a.memory.delay.or.a.spatial.transformation..and..2..reach.toward.visual.targets.displayed.on.the.monitor.using.a.computer.mouse..Together.these.tasks.should.take.1.1.5.hours.to.perform......Risks.and.Discomforts..We.do.not.foresee.any.risks.or.discomfort.from.your.participation.in.the.research.......Benefits.of.the.Research.and.Benefits.to.You..URPP.or.KURE.credits.if.applicable.....Voluntary.Participation..Your.participation.in.the.study.is.completely.voluntary.and.you.may.choose.to.stop.participating.at.any.time..Your.decision.not.to.volunteer.will.not.influence.your.relationship.with.us.or.anyone.else.at.York.University.either.now..or.in.the.future.....Withdrawal.from.the.Study..You.can.stop.participating.in.the.study.at.any.time..for.any.reason..if.you.so.decide..If.you.decide.to.stop.participating..you.will.still.be.eligible.to.receive.the.URPP.or.KURE.credit..if.applicable..for.agreeing.to.be.in.the.project..Your.decision.to.stop.participating..or.to.refuse.to.answer.particular.questions..will.not.affect.your.relationship.with.the.researchers..York.University..or.any.other.group.associated.with.this.project..In.the.event.you.withdraw.from.the.study..all.associated.data.collected.will.be.immediately.removed.from.our.computers.....Confidentiality..All.information.you.supply.and.recording.of.the.hand..mouse..movements.and.button.presses.during.the.experiment.will.be.held.in.confidence..your.name.will.not.appear.in.any.report..data.repository.or.publication.of.the.research..Your.data.will.be.safely.stored.on.password.protected.computers.in.our.locked.laboratory.and.only.research.staff.will.have.access.to.this.information..Your.personal.information.will.be.destroyed.after.the.study.has.been.published.or.when.5.years.have.expired.since.recording..The.recorded.experimental.data..such.as.mouse.cursor.movements..and.performance.on.spatial..perceptual.or.memory.tasks..will.be.shared..fully.anonymized..in.an.online.academic.data.repository..Open.Science.Framework.or.similar.in.the.interest.of.transparency.about.this.study..as.well.as.for.potential.use.in.future.studies.by.other.researchers..Confidentiality.will.be.provided.to.the.fullest.extent.possible.by.law.....Questions.About.the.Research..If.you.have.questions.about.the.research.in.general.or.about.your.role.in.the.study..please.feel.free.to.contact..Dr..Denise.Henriques.either.by.telephone.at..416..736.2100..extension.77215.or.by.e.mail..deniseh.yorku.ca...This.research.has.been.reviewed.and.approved.by.the.Human.Participants.Review.Sub.Committee..York.University.s.Ethics.Review.Board.and.conforms.to.the.standards.of.the.Canadian.Tri.Council.Research.Ethics.guidelines..If.you.have.any.questions.about.this.process..or.about.your.rights.as.a.participant.in.the.study..please.contact.the.Sr..Manager...Policy.Advisor.for.the.Office.of.Research.Ethics..5th.Floor..York.Research.Tower..York.University..telephone.416.736.5914.or.e.mail.ore.yorku.ca......Legal.Rights.and.Signatures.....I.______________________..consent.to.participate.in.this.study.conducted.by.Dr..Denise.Henriques.and.her.research.team..I.have.understood.the.nature.of.this.project.and.wish.to.participate..I.am.not.waiving.any.of.my.legal.rights.by.signing.this.form...My.signature.below.indicates.my.consent...............For.this.online.study..do.not.write.your.name..and.instead.of.signing..click.on.one.of.the.buttons.below.",
             "What.is.your.age.....Please.enter.a.valid.number..without.spaces.and.or.special.characters.",
             "Sex....Selected.Choice",
             "Are.you.currently.suffering.from.a.neurological.condition....Selected.Choice",
             "Are.you.currently.suffering.from.a.neurological.condition....Yes..Please.specify.if.comfortable...Text",
             "What.is.your.handedness.",
             "Do.you.need.to.wear.any.corrective.devices..e.g..glasses..contact.lenses..to.see.the.screen.",
             "Are.you.wearing.your.corrective.devices.right.now.OR.able.to.see.the.screen.well.enough.to.participate.",
             "On.a.scale.of.1.7..how.fit.physically.active.do.you.consider.yourself...1.not.physically.active.at.all.and.7.extremely.fit.",
             "On.a.scale.of.1.7..how.stressed.have.you.been.feeling.this.week...1.not.stressed.at.all..7.extremely.stressed.",
             "Do.you.use.opiates..or.other.recreational.drugs..besides.marijuana.or.alcohol...recreationally.",
             "Do.you.play.video.games.",
             "Have.you.ever.used.marijuana..e.g..weed..joint..hash..oil.etc..",
             "How.often.do.you.use.Marijuana..on.average.within.the.past.3.months..",
             "How.many.hours.of.sleep.did.you.get.last.night.....Please.enter.a.valid.number..without.spaces.and.or.special.characters.",
             "Have.you.ever.experienced.a.concussion.before.",
             "Do.you.play.a.musical.instrument.",
             "Were.there.any.problems.that.made.it.difficult.for.you.to.complete.this.study...i.e..in.pain..sick..sleepy..or.on.medication.that.may.impair.performance..aside.from.cannabis...Note..You.will.still.get.your.credit.payment.if.you.answer.yes.")

# keeping only the columns that will be used
q_full_1 <- q_full_1[columns]

names(q_full_1) <- c("id",
               "date_date",
               "finished",
               "informed_consent",
               "age",
               "sex",
               "neurological_conditions",
               "neurological_condition_description",
               "handedness",
               "glasses_contacts",
               "wearing_glasses_now",
               "physically_activity",
               "stressed",
               "opiates",
               "video_games",
               "used",
               "use_frequency",
               "sleep_last",
               "concussion",
               "music",
               "problems")

#### full part 2 ####

columns <- c("id",
             "date_date",
             "Finished",
             "Informed.Consent.Form...Study.Name..Visuomotor.learning....Researchers..Dr..Denise.Henriques....Purpose.of.the.Research...Our.research.team.is.interested.in.how.people.perceive..and.remember.the.location.and.characteristics.of.objects..and.how.they.move.a.mouse.cursor.to.visual.targets.under.various.circumstances.....What.You.Will.Be.Asked.to.Do.in.the.Research..You.will.be.asked.to.sit.in.front.of.a.computer.to.perceive..remember..discriminate.between.objects..and.or.acquire.targets..displayed.on.a.computer.monitor...These.short.tasks.can.include..1..detecting.objects.among.other.objects.or.changes.in.these.objects.after.a.memory.delay.or.a.spatial.transformation..and..2..reach.toward.visual.targets.displayed.on.the.monitor.using.a.computer.mouse..Together.these.tasks.should.take.1.1.5.hours.to.perform......Risks.and.Discomforts..We.do.not.foresee.any.risks.or.discomfort.from.your.participation.in.the.research.......Benefits.of.the.Research.and.Benefits.to.You..URPP.or.KURE.credits.if.applicable.....Voluntary.Participation..Your.participation.in.the.study.is.completely.voluntary.and.you.may.choose.to.stop.participating.at.any.time..Your.decision.not.to.volunteer.will.not.influence.your.relationship.with.us.or.anyone.else.at.York.University.either.now..or.in.the.future.....Withdrawal.from.the.Study..You.can.stop.participating.in.the.study.at.any.time..for.any.reason..if.you.so.decide..If.you.decide.to.stop.participating..you.will.still.be.eligible.to.receive.the.URPP.or.KURE.credit..if.applicable..for.agreeing.to.be.in.the.project..Your.decision.to.stop.participating..or.to.refuse.to.answer.particular.questions..will.not.affect.your.relationship.with.the.researchers..York.University..or.any.other.group.associated.with.this.project..In.the.event.you.withdraw.from.the.study..all.associated.data.collected.will.be.immediately.removed.from.our.computers.....Confidentiality..All.information.you.supply.and.recording.of.the.hand..mouse..movements.and.button.presses.during.the.experiment.will.be.held.in.confidence..your.name.will.not.appear.in.any.report..data.repository.or.publication.of.the.research..Your.data.will.be.safely.stored.on.password.protected.computers.in.our.locked.laboratory.and.only.research.staff.will.have.access.to.this.information..Your.personal.information.will.be.destroyed.after.the.study.has.been.published.or.when.5.years.have.expired.since.recording..The.recorded.experimental.data..such.as.mouse.cursor.movements..and.performance.on.spatial..perceptual.or.memory.tasks..will.be.shared..fully.anonymized..in.an.online.academic.data.repository..Open.Science.Framework.or.similar.in.the.interest.of.transparency.about.this.study..as.well.as.for.potential.use.in.future.studies.by.other.researchers..Confidentiality.will.be.provided.to.the.fullest.extent.possible.by.law.....Questions.About.the.Research..If.you.have.questions.about.the.research.in.general.or.about.your.role.in.the.study..please.feel.free.to.contact..Dr..Denise.Henriques.either.by.telephone.at..416..736.2100..extension.77215.or.by.e.mail..deniseh.yorku.ca...This.research.has.been.reviewed.and.approved.by.the.Human.Participants.Review.Sub.Committee..York.University.s.Ethics.Review.Board.and.conforms.to.the.standards.of.the.Canadian.Tri.Council.Research.Ethics.guidelines..If.you.have.any.questions.about.this.process..or.about.your.rights.as.a.participant.in.the.study..please.contact.the.Sr..Manager...Policy.Advisor.for.the.Office.of.Research.Ethics..5th.Floor..York.Research.Tower..York.University..telephone.416.736.5914.or.e.mail.ore.yorku.ca......Legal.Rights.and.Signatures.....I.______________________..consent.to.participate.in.this.study.conducted.by.Dr..Denise.Henriques.and.her.research.team..I.have.understood.the.nature.of.this.project.and.wish.to.participate..I.am.not.waiving.any.of.my.legal.rights.by.signing.this.form...My.signature.below.indicates.my.consent...............For.this.online.study..do.not.write.your.name..and.instead.of.signing..click.on.one.of.the.buttons.below.",
             "Do.you.need.to.wear.any.corrective.devices..e.g..glasses..contact.lenses..to.see.the.screen.",
             "Are.you.wearing.your.corrective.devices.right.now.OR.able.to.see.the.screen.well.enough.to.participate.",
             "On.a.scale.of.1.7..how.stressed.have.you.been.feeling.this.week...1.not.stressed.at.all..7.extremely.stressed.",
             "Do.you.use.opiates..or.other.recreational.drugs..besides.marijuana.or.alcohol...recreationally.",
             "Have.you.ever.used.marijuana..e.g..weed..joint..hash..oil.etc..",
             "How.often.do.you.use.Marijuana..on.average.within.the.past.3.months..",
             "How.many.hours.of.sleep.did.you.get.last.night.....Please.enter.a.valid.number..without.spaces.and.or.special.characters.",
             "Have.you.ever.experienced.a.concussion.before.",
             "Were.there.any.problems.that.made.it.difficult.for.you.to.complete.this.study...i.e..in.pain..sick..sleepy..or.on.medication.that.may.impair.performance..aside.from.cannabis...Note..You.will.still.get.your.credit.payment.if.you.answer.yes.")

# keeping only the columns that will be used
q_full_2 <- q_full_2[columns]

names(q_full_2) <- c("id",
               "date_date",
               "finished",
               "informed_consent",
               "glasses_contacts",
               "wearing_glasses_now",
               "stressed",
               "opiates",
               "used",
               "use_frequency",
               "sleep_last",
               "concussion",
               "problems")


#### q_ie_part1_fall ####

columns <- c("id",
             "date_date",
             "Finished",
             "Informed.Consent.Form...Study.Name..Visuomotor.learning....Researchers..Dr..Denise.Henriques....Purpose.of.the.Research...Our.research.team.is.interested.in.how.people.perceive..and.remember.the.location.and.characteristics.of.objects..and.how.they.move.a.mouse.cursor.to.visual.targets.under.various.circumstances.....What.You.Will.Be.Asked.to.Do.in.the.Research..You.will.be.asked.to.sit.in.front.of.a.computer.to.perceive..remember..discriminate.between.objects..and.or.acquire.targets..displayed.on.a.computer.monitor...These.short.tasks.can.include..1..detecting.objects.among.other.objects.or.changes.in.these.objects.after.a.memory.delay.or.a.spatial.transformation..and..2..reach.toward.visual.targets.displayed.on.the.monitor.using.a.computer.mouse..Together.these.tasks.should.take.1.1.5.hours.to.perform......Risks.and.Discomforts..We.do.not.foresee.any.risks.or.discomfort.from.your.participation.in.the.research.......Benefits.of.the.Research.and.Benefits.to.You..URPP.or.KURE.credits.if.applicable.....Voluntary.Participation..Your.participation.in.the.study.is.completely.voluntary.and.you.may.choose.to.stop.participating.at.any.time..Your.decision.not.to.volunteer.will.not.influence.your.relationship.with.us.or.anyone.else.at.York.University.either.now..or.in.the.future.....Withdrawal.from.the.Study..You.can.stop.participating.in.the.study.at.any.time..for.any.reason..if.you.so.decide..If.you.decide.to.stop.participating..you.will.still.be.eligible.to.receive.the.URPP.or.KURE.credit..if.applicable..for.agreeing.to.be.in.the.project..Your.decision.to.stop.participating..or.to.refuse.to.answer.particular.questions..will.not.affect.your.relationship.with.the.researchers..York.University..or.any.other.group.associated.with.this.project..In.the.event.you.withdraw.from.the.study..all.associated.data.collected.will.be.immediately.removed.from.our.computers.....Confidentiality..All.information.you.supply.and.recording.of.the.hand..mouse..movements.and.button.presses.during.the.experiment.will.be.held.in.confidence..your.name.will.not.appear.in.any.report..data.repository.or.publication.of.the.research..Your.data.will.be.safely.stored.on.password.protected.computers.in.our.locked.laboratory.and.only.research.staff.will.have.access.to.this.information..Your.personal.information.will.be.destroyed.after.the.study.has.been.published.or.when.5.years.have.expired.since.recording..The.recorded.experimental.data..such.as.mouse.cursor.movements..and.performance.on.spatial..perceptual.or.memory.tasks..will.be.shared..fully.anonymized..in.an.online.academic.data.repository..Open.Science.Framework.or.similar.in.the.interest.of.transparency.about.this.study..as.well.as.for.potential.use.in.future.studies.by.other.researchers..Confidentiality.will.be.provided.to.the.fullest.extent.possible.by.law.....Questions.About.the.Research..If.you.have.questions.about.the.research.in.general.or.about.your.role.in.the.study..please.feel.free.to.contact..Dr..Denise.Henriques.either.by.telephone.at..416..736.2100..extension.77215.or.by.e.mail..deniseh.yorku.ca...This.research.has.been.reviewed.and.approved.by.the.Human.Participants.Review.Sub.Committee..York.University.s.Ethics.Review.Board.and.conforms.to.the.standards.of.the.Canadian.Tri.Council.Research.Ethics.guidelines..If.you.have.any.questions.about.this.process..or.about.your.rights.as.a.participant.in.the.study..please.contact.the.Sr..Manager...Policy.Advisor.for.the.Office.of.Research.Ethics..5th.Floor..York.Research.Tower..York.University..telephone.416.736.5914.or.e.mail.ore.yorku.ca......Legal.Rights.and.Signatures.....I.______________________..consent.to.participate.in.this.study.conducted.by.Dr..Denise.Henriques.and.her.research.team..I.have.understood.the.nature.of.this.project.and.wish.to.participate..I.am.not.waiving.any.of.my.legal.rights.by.signing.this.form...My.signature.below.indicates.my.consent...............For.this.online.study..do.not.write.your.name..and.instead.of.signing..click.on.one.of.the.buttons.below.",
             "What.is.your.age.....Please.enter.a.valid.number..without.spaces.and.or.special.characters.",
             "What.sex.were.you.assigned.at.birth.",
             "Which.of.the.following.best.describes.your.ancestry..select.all.that.apply....Selected.Choice",
             "Which.of.the.following.best.describes.your.ancestry..select.all.that.apply....Other...Text",
             "What.best.describes.your.highest.level.of.education.",
             "Are.you.currently.suffering.from.a.neurological.condition....Selected.Choice",
             "Are.you.currently.suffering.from.a.neurological.condition....Yes..Please.specify.if.comfortable...Text",
             "What.is.your.handedness.",
             "Do.you.need.to.wear.any.corrective.devices..e.g..glasses..contact.lenses..to.see.the.screen.",
             "Are.you.wearing.your.corrective.devices.right.now.OR.able.to.see.the.screen.well.enough.to.participate.",
             "On.a.scale.of.1.7..how.fit.physically.active.do.you.consider.yourself...1.not.physically.active.at.all.and.7.extremely.fit.",
             "On.a.scale.of.1.7..how.stressed.have.you.been.feeling.this.week...1.not.stressed.at.all..7.extremely.stressed.",
             "Do.you.use.opiates..or.other.recreational.drugs..besides.marijuana.or.alcohol...recreationally.",
             "Do.you.play.video.games.",
             "Have.you.ever.used.marijuana..e.g..weed..joint..hash..oil.etc..",
             "How.often.do.you.use.Marijuana..on.average.within.the.past.3.months..",
             "How.many.hours.of.sleep.did.you.get.last.night.....Please.enter.a.valid.number..without.spaces.and.or.special.characters.",
             "Have.you.ever.experienced.a.concussion.before.",
             "Do.you.play.a.musical.instrument.",
             "Were.there.any.problems.that.made.it.difficult.for.you.to.complete.this.study...i.e..in.pain..sick..sleepy..or.on.medication.that.may.impair.performance..aside.from.cannabis...Note..You.will.still.get.your.credit.payment.if.you.answer.yes.")

# keeping only the columns that will be used
q_ie_part1_fall <- q_ie_part1_fall[columns]

names(q_ie_part1_fall) <- c("id",
               "date_date",
               "finished",
               "informed_consent",
               "age",
               "sex",
               "ancestry",
               "ancestry_other",
               "education",
               "neurological_conditions",
               "neurological_condition_description",
               "handedness",
               "glasses_contacts",
               "wearing_glasses_now",
               "physically_activity",
               "stressed",
               "opiates",
               "video_games",
               "used",
               "use_frequency",
               "sleep_last",
               "concussion",
               "music",
               "problems")

q_ie_part1_fall$id <- as.character(q_ie_part1_fall$id)

#### q_ie_part1_winter ####

columns <- c("id",
             "date_date",
             "Finished",
             "Informed.Consent.Form...Study.Name..Visuomotor.learning....Researchers..Dr..Denise.Henriques....Purpose.of.the.Research...Our.research.team.is.interested.in.how.people.perceive..and.remember.the.location.and.characteristics.of.objects..and.how.they.move.a.mouse.cursor.to.visual.targets.under.various.circumstances.....What.You.Will.Be.Asked.to.Do.in.the.Research..You.will.be.asked.to.sit.in.front.of.a.computer.to.perceive..remember..discriminate.between.objects..and.or.acquire.targets..displayed.on.a.computer.monitor...These.short.tasks.can.include..1..detecting.objects.among.other.objects.or.changes.in.these.objects.after.a.memory.delay.or.a.spatial.transformation..and..2..reach.toward.visual.targets.displayed.on.the.monitor.using.a.computer.mouse..Together.these.tasks.should.take.1.1.5.hours.to.perform......Risks.and.Discomforts..We.do.not.foresee.any.risks.or.discomfort.from.your.participation.in.the.research.......Benefits.of.the.Research.and.Benefits.to.You..URPP.or.KURE.credits.if.applicable.....Voluntary.Participation..Your.participation.in.the.study.is.completely.voluntary.and.you.may.choose.to.stop.participating.at.any.time..Your.decision.not.to.volunteer.will.not.influence.your.relationship.with.us.or.anyone.else.at.York.University.either.now..or.in.the.future.....Withdrawal.from.the.Study..You.can.stop.participating.in.the.study.at.any.time..for.any.reason..if.you.so.decide..If.you.decide.to.stop.participating..you.will.still.be.eligible.to.receive.the.URPP.or.KURE.credit..if.applicable..for.agreeing.to.be.in.the.project..Your.decision.to.stop.participating..or.to.refuse.to.answer.particular.questions..will.not.affect.your.relationship.with.the.researchers..York.University..or.any.other.group.associated.with.this.project..In.the.event.you.withdraw.from.the.study..all.associated.data.collected.will.be.immediately.removed.from.our.computers.....Confidentiality..All.information.you.supply.and.recording.of.the.hand..mouse..movements.and.button.presses.during.the.experiment.will.be.held.in.confidence..your.name.will.not.appear.in.any.report..data.repository.or.publication.of.the.research..Your.data.will.be.safely.stored.on.password.protected.computers.in.our.locked.laboratory.and.only.research.staff.will.have.access.to.this.information..Your.personal.information.will.be.destroyed.after.the.study.has.been.published.or.when.5.years.have.expired.since.recording..The.recorded.experimental.data..such.as.mouse.cursor.movements..and.performance.on.spatial..perceptual.or.memory.tasks..will.be.shared..fully.anonymized..in.an.online.academic.data.repository..Open.Science.Framework.or.similar.in.the.interest.of.transparency.about.this.study..as.well.as.for.potential.use.in.future.studies.by.other.researchers..Confidentiality.will.be.provided.to.the.fullest.extent.possible.by.law.....Questions.About.the.Research..If.you.have.questions.about.the.research.in.general.or.about.your.role.in.the.study..please.feel.free.to.contact..Dr..Denise.Henriques.either.by.telephone.at..416..736.2100..extension.77215.or.by.e.mail..deniseh.yorku.ca...This.research.has.been.reviewed.and.approved.by.the.Human.Participants.Review.Sub.Committee..York.University.s.Ethics.Review.Board.and.conforms.to.the.standards.of.the.Canadian.Tri.Council.Research.Ethics.guidelines..If.you.have.any.questions.about.this.process..or.about.your.rights.as.a.participant.in.the.study..please.contact.the.Sr..Manager...Policy.Advisor.for.the.Office.of.Research.Ethics..5th.Floor..York.Research.Tower..York.University..telephone.416.736.5914.or.e.mail.ore.yorku.ca......Legal.Rights.and.Signatures.....I.______________________..consent.to.participate.in.this.study.conducted.by.Dr..Denise.Henriques.and.her.research.team..I.have.understood.the.nature.of.this.project.and.wish.to.participate..I.am.not.waiving.any.of.my.legal.rights.by.signing.this.form...My.signature.below.indicates.my.consent...............For.this.online.study..do.not.write.your.name..and.instead.of.signing..click.on.one.of.the.buttons.below.",
             "What.is.your.age.....Please.enter.a.valid.number..without.spaces.and.or.special.characters.",
             "What.sex.were.you.assigned.at.birth.",
             "Which.of.the.following.best.describes.your.ancestry..select.all.that.apply....Selected.Choice",
             "Which.of.the.following.best.describes.your.ancestry..select.all.that.apply....Other...Text",
             "What.best.describes.your.highest.level.of.education.",
             "Are.you.currently.suffering.from.a.neurological.condition.",
             "Which.of.the.following.neurological.conditions.are.you.currently.suffering.from...Please.select.all.that.apply....Selected.Choice",
             "Which.of.the.following.neurological.conditions.are.you.currently.suffering.from...Please.select.all.that.apply....Other..please.specify.if.comfortable....Text",
             "What.is.your.handedness.",
             "Do.you.need.to.wear.any.corrective.devices..e.g..glasses..contact.lenses..to.see.the.screen.",
             "Are.you.wearing.your.corrective.devices.right.now.OR.able.to.see.the.screen.well.enough.to.participate.",
             "On.a.scale.of.1.7..how.fit.physically.active.do.you.consider.yourself...1.not.physically.active.at.all.and.7.extremely.fit.",
             "On.a.scale.of.1.7..how.stressed.have.you.been.feeling.this.week...1.not.stressed.at.all..7.extremely.stressed.",
             "Do.you.use.opiates..or.other.recreational.drugs..besides.marijuana.or.alcohol...recreationally.",
             "Do.you.play.video.games.",
             "Have.you.ever.used.marijuana..e.g..weed..joint..hash..oil.etc..",
             "How.often.do.you.use.Marijuana..on.average.within.the.past.3.months..",
             "Have.you.ever.experienced.a.concussion.before.",
             "Do.you.play.a.musical.instrument.",
             "Were.there.any.problems.that.made.it.difficult.for.you.to.complete.this.study...i.e..in.pain..sick..sleepy..or.on.medication.that.may.impair.performance..aside.from.cannabis...Note..You.will.still.get.your.credit.payment.if.you.answer.yes.")

# keeping only the columns that will be used
q_ie_part1_winter <- q_ie_part1_winter[columns]

names(q_ie_part1_winter) <- c("id",
               "date_date",
               "finished",
               "informed_consent",
               "age",
               "sex",
               "ancestry",
               "ancestry_other",
               "education",
               "neurological_conditions",
               "neurological_condition_choice",
               "neurological_condition_description",
               "handedness",
               "glasses_contacts",
               "wearing_glasses_now",
               "physically_activity",
               "stressed",
               "opiates",
               "video_games",
               "used",
               "use_frequency",
               "concussion",
               "music",
               "problems")

q_ie_part1_winter$id <- as.character(q_ie_part1_winter$id)

#### q_ie_part2_fall ####

columns <- c("id",
             "date_date",
             "Finished",
             "Informed.Consent.Form...Study.Name..Visuomotor.learning....Researchers..Dr..Denise.Henriques....Purpose.of.the.Research...Our.research.team.is.interested.in.how.people.perceive..and.remember.the.location.and.characteristics.of.objects..and.how.they.move.a.mouse.cursor.to.visual.targets.under.various.circumstances.....What.You.Will.Be.Asked.to.Do.in.the.Research..You.will.be.asked.to.sit.in.front.of.a.computer.to.perceive..remember..discriminate.between.objects..and.or.acquire.targets..displayed.on.a.computer.monitor...These.short.tasks.can.include..1..detecting.objects.among.other.objects.or.changes.in.these.objects.after.a.memory.delay.or.a.spatial.transformation..and..2..reach.toward.visual.targets.displayed.on.the.monitor.using.a.computer.mouse..Together.these.tasks.should.take.1.1.5.hours.to.perform......Risks.and.Discomforts..We.do.not.foresee.any.risks.or.discomfort.from.your.participation.in.the.research.......Benefits.of.the.Research.and.Benefits.to.You..URPP.or.KURE.credits.if.applicable.....Voluntary.Participation..Your.participation.in.the.study.is.completely.voluntary.and.you.may.choose.to.stop.participating.at.any.time..Your.decision.not.to.volunteer.will.not.influence.your.relationship.with.us.or.anyone.else.at.York.University.either.now..or.in.the.future.....Withdrawal.from.the.Study..You.can.stop.participating.in.the.study.at.any.time..for.any.reason..if.you.so.decide..If.you.decide.to.stop.participating..you.will.still.be.eligible.to.receive.the.URPP.or.KURE.credit..if.applicable..for.agreeing.to.be.in.the.project..Your.decision.to.stop.participating..or.to.refuse.to.answer.particular.questions..will.not.affect.your.relationship.with.the.researchers..York.University..or.any.other.group.associated.with.this.project..In.the.event.you.withdraw.from.the.study..all.associated.data.collected.will.be.immediately.removed.from.our.computers.....Confidentiality..All.information.you.supply.and.recording.of.the.hand..mouse..movements.and.button.presses.during.the.experiment.will.be.held.in.confidence..your.name.will.not.appear.in.any.report..data.repository.or.publication.of.the.research..Your.data.will.be.safely.stored.on.password.protected.computers.in.our.locked.laboratory.and.only.research.staff.will.have.access.to.this.information..Your.personal.information.will.be.destroyed.after.the.study.has.been.published.or.when.5.years.have.expired.since.recording..The.recorded.experimental.data..such.as.mouse.cursor.movements..and.performance.on.spatial..perceptual.or.memory.tasks..will.be.shared..fully.anonymized..in.an.online.academic.data.repository..Open.Science.Framework.or.similar.in.the.interest.of.transparency.about.this.study..as.well.as.for.potential.use.in.future.studies.by.other.researchers..Confidentiality.will.be.provided.to.the.fullest.extent.possible.by.law.....Questions.About.the.Research..If.you.have.questions.about.the.research.in.general.or.about.your.role.in.the.study..please.feel.free.to.contact..Dr..Denise.Henriques.either.by.telephone.at..416..736.2100..extension.77215.or.by.e.mail..deniseh.yorku.ca...This.research.has.been.reviewed.and.approved.by.the.Human.Participants.Review.Sub.Committee..York.University.s.Ethics.Review.Board.and.conforms.to.the.standards.of.the.Canadian.Tri.Council.Research.Ethics.guidelines..If.you.have.any.questions.about.this.process..or.about.your.rights.as.a.participant.in.the.study..please.contact.the.Sr..Manager...Policy.Advisor.for.the.Office.of.Research.Ethics..5th.Floor..York.Research.Tower..York.University..telephone.416.736.5914.or.e.mail.ore.yorku.ca......Legal.Rights.and.Signatures.....I.______________________..consent.to.participate.in.this.study.conducted.by.Dr..Denise.Henriques.and.her.research.team..I.have.understood.the.nature.of.this.project.and.wish.to.participate..I.am.not.waiving.any.of.my.legal.rights.by.signing.this.form...My.signature.below.indicates.my.consent...............For.this.online.study..do.not.write.your.name..and.instead.of.signing..click.on.one.of.the.buttons.below.",
             "Do.you.need.to.wear.any.corrective.devices..e.g..glasses..contact.lenses..to.see.the.screen.",
             "Are.you.wearing.your.corrective.devices.right.now.OR.able.to.see.the.screen.well.enough.to.participate.",
             "On.a.scale.of.1.7..how.stressed.have.you.been.feeling.this.week...1.not.stressed.at.all..7.extremely.stressed.",
             "Do.you.use.opiates..or.other.recreational.drugs..besides.marijuana.or.alcohol...recreationally.",
             "Have.you.ever.used.marijuana..e.g..weed..joint..hash..oil.etc..",
             "How.often.do.you.use.Marijuana..on.average.within.the.past.3.months..",
             "How.many.hours.of.sleep.did.you.get.last.night.....Please.enter.a.valid.number..without.spaces.and.or.special.characters.",
             "Have.you.ever.experienced.a.concussion.before.",
             "Were.there.any.problems.that.made.it.difficult.for.you.to.complete.this.study...i.e..in.pain..sick..sleepy..or.on.medication.that.may.impair.performance..aside.from.cannabis...Note..You.will.still.get.your.credit.payment.if.you.answer.yes.")

# keeping only the columns that will be used
q_ie_part2_fall <- q_ie_part2_fall[columns]

names(q_ie_part2_fall) <- c("id",
                            "date_date",
                            "finished",
                            "informed_consent",
                            "glasses_contacts",
                            "wearing_glasses_now",
                            "stressed",
                            "opiates",
                            "used",
                            "use_frequency",
                            "sleep_last",
                            "concussion",
                            "problems")

q_ie_part2_fall$id <- as.character(q_ie_part2_fall$id)

#### q_ie_part2_winter ####

columns <- c("id",
             "date_date",
             "Finished",
             "Informed.Consent.Form...Study.Name..Visuomotor.learning....Researchers..Dr..Denise.Henriques....Purpose.of.the.Research...Our.research.team.is.interested.in.how.people.perceive..and.remember.the.location.and.characteristics.of.objects..and.how.they.move.a.mouse.cursor.to.visual.targets.under.various.circumstances.....What.You.Will.Be.Asked.to.Do.in.the.Research..You.will.be.asked.to.sit.in.front.of.a.computer.to.perceive..remember..discriminate.between.objects..and.or.acquire.targets..displayed.on.a.computer.monitor...These.short.tasks.can.include..1..detecting.objects.among.other.objects.or.changes.in.these.objects.after.a.memory.delay.or.a.spatial.transformation..and..2..reach.toward.visual.targets.displayed.on.the.monitor.using.a.computer.mouse..Together.these.tasks.should.take.1.1.5.hours.to.perform......Risks.and.Discomforts..We.do.not.foresee.any.risks.or.discomfort.from.your.participation.in.the.research.......Benefits.of.the.Research.and.Benefits.to.You..URPP.or.KURE.credits.if.applicable.....Voluntary.Participation..Your.participation.in.the.study.is.completely.voluntary.and.you.may.choose.to.stop.participating.at.any.time..Your.decision.not.to.volunteer.will.not.influence.your.relationship.with.us.or.anyone.else.at.York.University.either.now..or.in.the.future.....Withdrawal.from.the.Study..You.can.stop.participating.in.the.study.at.any.time..for.any.reason..if.you.so.decide..If.you.decide.to.stop.participating..you.will.still.be.eligible.to.receive.the.URPP.or.KURE.credit..if.applicable..for.agreeing.to.be.in.the.project..Your.decision.to.stop.participating..or.to.refuse.to.answer.particular.questions..will.not.affect.your.relationship.with.the.researchers..York.University..or.any.other.group.associated.with.this.project..In.the.event.you.withdraw.from.the.study..all.associated.data.collected.will.be.immediately.removed.from.our.computers.....Confidentiality..All.information.you.supply.and.recording.of.the.hand..mouse..movements.and.button.presses.during.the.experiment.will.be.held.in.confidence..your.name.will.not.appear.in.any.report..data.repository.or.publication.of.the.research..Your.data.will.be.safely.stored.on.password.protected.computers.in.our.locked.laboratory.and.only.research.staff.will.have.access.to.this.information..Your.personal.information.will.be.destroyed.after.the.study.has.been.published.or.when.5.years.have.expired.since.recording..The.recorded.experimental.data..such.as.mouse.cursor.movements..and.performance.on.spatial..perceptual.or.memory.tasks..will.be.shared..fully.anonymized..in.an.online.academic.data.repository..Open.Science.Framework.or.similar.in.the.interest.of.transparency.about.this.study..as.well.as.for.potential.use.in.future.studies.by.other.researchers..Confidentiality.will.be.provided.to.the.fullest.extent.possible.by.law.....Questions.About.the.Research..If.you.have.questions.about.the.research.in.general.or.about.your.role.in.the.study..please.feel.free.to.contact..Dr..Denise.Henriques.either.by.telephone.at..416..736.2100..extension.77215.or.by.e.mail..deniseh.yorku.ca...This.research.has.been.reviewed.and.approved.by.the.Human.Participants.Review.Sub.Committee..York.University.s.Ethics.Review.Board.and.conforms.to.the.standards.of.the.Canadian.Tri.Council.Research.Ethics.guidelines..If.you.have.any.questions.about.this.process..or.about.your.rights.as.a.participant.in.the.study..please.contact.the.Sr..Manager...Policy.Advisor.for.the.Office.of.Research.Ethics..5th.Floor..York.Research.Tower..York.University..telephone.416.736.5914.or.e.mail.ore.yorku.ca......Legal.Rights.and.Signatures.....I.______________________..consent.to.participate.in.this.study.conducted.by.Dr..Denise.Henriques.and.her.research.team..I.have.understood.the.nature.of.this.project.and.wish.to.participate..I.am.not.waiving.any.of.my.legal.rights.by.signing.this.form...My.signature.below.indicates.my.consent...............For.this.online.study..do.not.write.your.name..and.instead.of.signing..click.on.one.of.the.buttons.below.",
             "Do.you.need.to.wear.any.corrective.devices..e.g..glasses..contact.lenses..to.see.the.screen.",
             "Are.you.wearing.your.corrective.devices.right.now.OR.able.to.see.the.screen.well.enough.to.participate.",
             "On.a.scale.of.1.7..how.stressed.have.you.been.feeling.this.week...1.not.stressed.at.all..7.extremely.stressed.",
             "Do.you.use.opiates..or.other.recreational.drugs..besides.marijuana.or.alcohol...recreationally.",
             "Have.you.ever.used.marijuana..e.g..weed..joint..hash..oil.etc..",
             "How.often.do.you.use.Marijuana..on.average.within.the.past.3.months..",
             "Have.you.ever.experienced.a.concussion.before.",
             "Were.there.any.problems.that.made.it.difficult.for.you.to.complete.this.study...i.e..in.pain..sick..sleepy..or.on.medication.that.may.impair.performance..aside.from.cannabis...Note..You.will.still.get.your.credit.payment.if.you.answer.yes.")

# keeping only the columns that will be used
q_ie_part2_winter <- q_ie_part2_winter[columns]

names(q_ie_part2_winter) <- c("id",
                            "date_date",
                            "finished",
                            "informed_consent",
                            "glasses_contacts",
                            "wearing_glasses_now",
                            "stressed",
                            "opiates",
                            "used",
                            "use_frequency",
                            "concussion",
                            "problems")

q_ie_part2_winter$id <- as.character(q_ie_part2_winter$id)

#### q_ie_part3_fall ####

names(q_ie_part3_fall)[18] <- "Consent"
names(q_ie_part3_fall)[173] <- "Consent2"

columns <- c("id",
             "date_date",
             "Finished",
             "Consent",
             "Consent2",
             "What.is.your.highest.level.of.education.",
             "Are.you.currently.suffering.from.a.neurological.condition....Selected.Choice",
             "Are.you.currently.suffering.from.a.neurological.condition....Yes..please.specify..if.comfortable....Text",
             "Do.you.need.to.wear.any.corrective.devices..e.g..glasses..contact.lenses..to.see.the.screen.",
             "Are.you.wearing.your.corrective.devices.right.now.OR.able.to.see.the.screen.well.enough.to.participate.",
             "On.a.scale.of.1.7..how.stressed.have.you.been.feeling.this.week...1.not.stressed.at.all..7.extremely.stressed.",
             "Do.you.use.opiates..or.other.recreational.drugs..besides.marijuana.or.alcohol...recreationally.",
             "How.many.hours.of.sleep.did.you.get.last.night.....Please.enter.a.valid.number..without.spaces.and.or.special.characters.",
             "Have.you.ever.experienced.a.concussion.before.",
             "Please.rate.how.high.you.feel.at.this.moment....0...Not.High....100...Highest.I.ve.ever.been.on.Marijuana",
             "Please.rate.how.high.you.feel.at.this.moment....0...Not.High....100...Highest.I.ve.ever.been.on.Marijuana.1",
             "Please.rate.how.high.you.feel.at.this.moment....0...Not.High....100...Highest.I.ve.ever.been.on.Marijuana.2",
             "Please.rate.how.high.you.feel.at.this.moment....0...Not.High.....100...Highest.I.ve.ever.been.on.Marijuana",
             "Were.there.any.problems.that.made.it.difficult.for.you.to.complete.this.study...i.e..in.pain..sick..sleepy..or.on.medication.that.may.impair.performance..aside.from.cannabis...Note..You.will.still.get.your.credit.payment.if.you.answer.yes.")

# keeping only the columns that will be used
q_ie_part3_fall <- q_ie_part3_fall[columns]

names(q_ie_part3_fall) <- c("id",
               "date_date",
               "finished",
               "informed_consent",
               "informed_consent2",
               "education",
               "neurological_conditions",
               "neurological_condition_description",
               "glasses_contacts",
               "wearing_glasses_now",
               "stressed",
               "opiates",
               "sleep_last",
               "concussion",
               "how_high1",
               "how_high2",
               "how_high3",
               "how_high4",
               "problems")

#### q_ie_part3_winter ####

names(q_ie_part3_winter)[18] <- "Consent"
names(q_ie_part3_winter)[189] <- "Consent2"

columns <- c("id",
             "date_date",
             "Finished",
             "Consent",
             "Consent2",
             "What.is.your.highest.level.of.education.",
             "Are.you.currently.suffering.from.a.neurological.condition.",
             "Which.of.the.following.neurological.conditions.are.you.currently.suffering.from...Please.select.all.that.apply....Selected.Choice",
             "Which.of.the.following.neurological.conditions.are.you.currently.suffering.from...Please.select.all.that.apply....Other..please.specify.if.comfortable....Text",
             "Do.you.need.to.wear.any.corrective.devices..e.g..glasses..contact.lenses..to.see.the.screen.",
             "Are.you.wearing.your.corrective.devices.right.now.OR.able.to.see.the.screen.well.enough.to.participate.",
             "On.a.scale.of.1.7..how.stressed.have.you.been.feeling.this.week...1.not.stressed.at.all..7.extremely.stressed.",
             "Do.you.use.opiates..or.other.recreational.drugs..besides.marijuana.or.alcohol...recreationally.",
             "Have.you.ever.experienced.a.concussion.before.",
             "Please.rate.how.high.you.feel.at.this.moment....0...Not.High....100...Highest.I.ve.ever.been.on.Marijuana",
             "Please.rate.how.high.you.feel.at.this.moment....0...Not.High....100...Highest.I.ve.ever.been.on.Marijuana.1",
             "Please.rate.how.high.you.feel.at.this.moment....0...Not.High....100...Highest.I.ve.ever.been.on.Marijuana.2",
             "Please.rate.how.high.you.feel.at.this.moment....0...Not.High.....100...Highest.I.ve.ever.been.on.Marijuana",
             "Were.there.any.problems.that.made.it.difficult.for.you.to.complete.this.study...i.e..in.pain..sick..sleepy..or.on.medication.that.may.impair.performance..aside.from.cannabis...Note..You.will.still.get.your.credit.payment.if.you.answer.yes.")

# keeping only the columns that will be used
q_ie_part3_winter <- q_ie_part3_winter[columns]

names(q_ie_part3_winter) <- c("id",
               "date_date",
               "finished",
               "informed_consent",
               "informed_consent2",
               "education",
               "neurological_conditions",
               "neurological_condition_choice",
               "neurological_condition_description",
               "glasses_contacts",
               "wearing_glasses_now",
               "stressed",
               "opiates",
               "concussion",
               "how_high1",
               "how_high2",
               "how_high3",
               "how_high4",
               "problems")

#### q_ie_part4_fall ####

names(q_ie_part4_fall)[18] <- "Consent"
names(q_ie_part4_fall)[172] <- "Consent2"

columns <- c("id",
             "date_date",
             "Finished",
             "Consent",
             "Consent2",
             "What.is.your.highest.level.of.education.",
             "Are.you.currently.suffering.from.a.neurological.condition....Selected.Choice",
             "Are.you.currently.suffering.from.a.neurological.condition....Yes..please.specify..if.comfortable....Text",
             "Do.you.need.to.wear.any.corrective.devices..e.g..glasses..contact.lenses..to.see.the.screen.",
             "Are.you.wearing.your.corrective.devices.right.now.OR.able.to.see.the.screen.well.enough.to.participate.",
             "On.a.scale.of.1.7..how.stressed.have.you.been.feeling.this.week...1.not.stressed.at.all..7.extremely.stressed.",
             "Do.you.use.opiates..or.other.recreational.drugs..besides.marijuana.or.alcohol...recreationally.",
             "How.many.hours.of.sleep.did.you.get.last.night.....Please.enter.a.valid.number..without.spaces.and.or.special.characters.",
             "Have.you.ever.experienced.a.concussion.before.",
             "Please.rate.how.high.you.feel.at.this.moment....0...Not.High....100...Highest.I.ve.ever.been.on.Marijuana",
             "Please.rate.how.high.you.feel.at.this.moment....0...Not.High....100...Highest.I.ve.ever.been.on.Marijuana.1",
             "Please.rate.how.high.you.feel.at.this.moment....0...Not.High....100...Highest.I.ve.ever.been.on.Marijuana.2",
             "Please.rate.how.high.you.feel.at.this.moment....0...Not.high.....100...Highest.I.ve.ever.been.on.Marijuana",
             "Were.there.any.problems.that.made.it.difficult.for.you.to.complete.this.study...i.e..in.pain..sick..sleepy..or.on.medication.that.may.impair.performance..aside.from.cannabis...Note..You.will.still.get.your.credit.payment.if.you.answer.yes.")

# keeping only the columns that will be used
q_ie_part4_fall <- q_ie_part4_fall[columns]

names(q_ie_part4_fall) <- c("id",
                            "date_date",
                            "finished",
                            "informed_consent",
                            "informed_consent2",
                            "education",
                            "neurological_conditions",
                            "neurological_condition_description",
                            "glasses_contacts",
                            "wearing_glasses_now",
                            "stressed",
                            "opiates",
                            "sleep_last",
                            "concussion",
                            "how_high1",
                            "how_high2",
                            "how_high3",
                            "how_high4",
                            "problems")

#### q_ie_part4_winter ####

names(q_ie_part4_winter)[18] <- "Consent"
names(q_ie_part4_winter)[186] <- "Consent2"

columns <- c("id",
             "date_date",
             "Finished",
             "Consent",
             "Consent2",
             "What.is.your.highest.level.of.education.",
             "Are.you.currently.suffering.from.a.neurological.condition.",
             "Which.of.the.following.neurological.conditions.are.you.currently.suffering.from...Please.select.all.that.apply....Selected.Choice",
             "Which.of.the.following.neurological.conditions.are.you.currently.suffering.from...Please.select.all.that.apply....Other..please.specify.if.comfortable....Text",
             "Do.you.need.to.wear.any.corrective.devices..e.g..glasses..contact.lenses..to.see.the.screen.",
             "Are.you.wearing.your.corrective.devices.right.now.OR.able.to.see.the.screen.well.enough.to.participate.",
             "On.a.scale.of.1.7..how.stressed.have.you.been.feeling.this.week...1.not.stressed.at.all..7.extremely.stressed.",
             "Do.you.use.opiates..or.other.recreational.drugs..besides.marijuana.or.alcohol...recreationally.",
             "Have.you.ever.experienced.a.concussion.before.",
             "Please.rate.how.high.you.feel.at.this.moment....0...Not.High....100...Highest.I.ve.ever.been.on.Marijuana",
             "Please.rate.how.high.you.feel.at.this.moment....0...Not.High....100...Highest.I.ve.ever.been.on.Marijuana.1",
             "Please.rate.how.high.you.feel.at.this.moment....0...Not.High....100...Highest.I.ve.ever.been.on.Marijuana.2",
             "Please.rate.how.high.you.feel.at.this.moment....0...Not.high.....100...Highest.I.ve.ever.been.on.Marijuana",
             "Were.there.any.problems.that.made.it.difficult.for.you.to.complete.this.study...i.e..in.pain..sick..sleepy..or.on.medication.that.may.impair.performance..aside.from.cannabis...Note..You.will.still.get.your.credit.payment.if.you.answer.yes.")

# keeping only the columns that will be used
q_ie_part4_winter <- q_ie_part4_winter[columns]

names(q_ie_part4_winter) <- c("id",
                            "date_date",
                            "finished",
                            "informed_consent",
                            "informed_consent2",
                            "education",
                            "neurological_conditions",
                            "neurological_condition_choice",
                            "neurological_condition_description",
                            "glasses_contacts",
                            "wearing_glasses_now",
                            "stressed",
                            "opiates",
                            "concussion",
                            "how_high1",
                            "how_high2",
                            "how_high3",
                            "how_high4",
                            "problems")

q_ie_part4_winter$id <- as.character(q_ie_part4_winter$id)

#### combine all questionnaires into one ####

df_combined <- bind_rows(q_full_1, q_full_2)

#do those separately
#df_combined <- bind_rows(df_combined, q_ie_part1_fall)
#df_combined <- bind_rows(df_combined, q_ie_part1_winter)
#df_combined <- bind_rows(df_combined, q_ie_part2_fall)
#df_combined <- bind_rows(df_combined, q_ie_part2_winter)
#df_combined <- bind_rows(df_combined, q_ie_part3_fall)
#df_combined <- bind_rows(df_combined, q_ie_part3_winter)
#df_combined <- bind_rows(df_combined, q_ie_part4_fall)
#df_combined <- bind_rows(df_combined, q_ie_part4_winter)

#### subset each ####

# replace NA in column in informed consent with values from column informed_consent2
df_combined$informed_consent <- ifelse(df_combined$informed_consent == "", NA, df_combined$informed_consent)
#df_combined$informed_consent <- ifelse(is.na(df_combined$informed_consent), df_combined$informed_consent2, df_combined$informed_consent)

# keep those who agreed to participate
df_combined <- df_combined[which(df_combined$informed_consent == 'I agree to participate in this study'),]

#df_combined <- df_combined %>% 
#  select(-informed_consent2)

# remove those who need to wear corrective devices to see screen and not wearing them now (33)
df_combined <- df_combined[which(df_combined$wearing_glasses_now != 'No'),]

df_combined <- df_combined %>% 
  select(-glasses_contacts, -wearing_glasses_now)

# keep those who have finished the session
df_combined <- df_combined[which(df_combined$finished == 'TRUE'),]

df_combined <- df_combined %>% 
  select(-finished)

# fill NA with other value in the same column informed consent and same id
df_combined$neurological_conditions <- ifelse(df_combined$neurological_conditions == "", NA, df_combined$neurological_conditions)

df_combined <- df_combined %>% 
  group_by(id) %>% 
  fill(neurological_conditions, .direction = "down")

# remove people with neurological conditions, except migraine
df_combined <- subset(df_combined, (neurological_conditions == "No" | 
                                      neurological_condition_description == "headache and migraine " | 
                                      neurological_condition_description == "Migraines" |
                                      neurological_condition_description == "Chronic Migraines" |
                                      neurological_condition_description == "Chronic Migraines " |
                                      neurological_condition_description == "no" |
                                      neurological_condition_description == "No" |
                                      neurological_condition_description == "none"))

#df_combined <- df_combined %>% 
#  select(-neurological_condition_choice)

df_combined <- df_combined %>% 
  select(-neurological_conditions, -neurological_condition_description)

# remove opiate users
df_combined <- df_combined[which(df_combined$opiates == "No"),]

df_combined <- df_combined %>% 
  select(-opiates)

# remove those that didn't passed screening
#df <- df[which(df$passedscreening == "TRUE"),]

## fill in sex and age

df_combined <- df_combined %>% 
  group_by(id) %>% 
  fill(sex, .direction = "down")

df_combined$year <- year(df_combined$date_date)

df_combined <- df_combined %>%
  mutate(year_of_birth = year - age)

# Replace -Inf with NA in column year_of_birth
df_combined$year_of_birth <- replace(df_combined$year_of_birth , 
                                     is.infinite(df_combined$year_of_birth ), NA)

# group by id and replace year_of_birth with latest year
df_combined <- df_combined %>%
  group_by(id) %>%
  mutate(year_of_birth = if(all(is.na(year_of_birth))) NA else max(year_of_birth, na.rm = TRUE)) %>%
  ungroup()

# check if there are discrepancies
discrepancies <- df_combined %>%
  group_by(id) %>%
  filter(n_distinct(year_of_birth, na.rm = TRUE) > 1)

df_combined <- df_combined %>% 
  select(-age)

# check if there are discrepancies by sex
discrepancies <- df_combined %>%
  group_by(id) %>%
  filter(n_distinct(sex, na.rm = TRUE) > 1)

# remove 216499, as it is not clear what sex the respondent is
df_combined <- df_combined %>% 
  filter(id != "216499")

# change one "Other" response to usual response by the respondent in "221686"
df_combined <- df_combined %>%
  mutate(sex = if_else(id == "221686", "Female", sex))

# fill the handedness
df_combined <- df_combined %>% 
  group_by(id) %>% 
  fill(handedness, .direction = "down")

# many missing, so I don't think this variable will be useful
df_combined <- df_combined %>% 
  select(-handedness)

# fill in the physical activity by max
df_combined <- df_combined %>%
  group_by(id) %>%
  mutate(physically_activity = ifelse(all(is.na(physically_activity)), NA_real_, mean(physically_activity, na.rm = TRUE))) %>%
  ungroup()

# check if there are discrepancies
discrepancies <- df_combined %>%
  group_by(id) %>%
  filter(n_distinct(physically_activity, na.rm = TRUE) > 1)

# fill in the stress by down
df_combined <- df_combined %>% 
  group_by(id) %>% 
  fill(stressed, .direction = "down")

# change stressed to average reported
df_combined <- df_combined %>%
  group_by(id) %>%
  mutate(stressed = ifelse(all(is.na(stressed)), NA_real_, mean(stressed, na.rm = TRUE))) %>%
  ungroup()

# fill the videogames by down
df_combined <- df_combined %>% 
  group_by(id) %>% 
  fill(video_games, .direction = "down")

# check if there are discrepancies
discrepancies <- df_combined %>%
  group_by(id) %>%
  filter(n_distinct(video_games, na.rm = TRUE) > 1)

# change to yes's if at least once responded as yes
df_combined <- df_combined %>% 
  group_by(id) %>% 
  mutate(video_games = ifelse("Yes" %in% video_games, "Yes", video_games)) %>%
  ungroup()

# change sleep time to average reported
df_combined <- df_combined %>%
  group_by(id) %>%
  mutate(sleep_last = ifelse(all(is.na(sleep_last)), NA_real_, mean(sleep_last, na.rm = TRUE))) %>%
  ungroup()

# fill the concussions by down
df_combined <- df_combined %>% 
  group_by(id) %>% 
  fill(concussion, .direction = "down")

# check discrepancies
discrepancies <- df_combined %>%
  group_by(id) %>%
  filter(n_distinct(concussion, na.rm = TRUE) > 1)

# change to yes's if at least once responded as yes
df_combined <- df_combined %>% 
  group_by(id) %>% 
  mutate(concussion = ifelse("Yes" %in% concussion, "Yes", concussion)) %>%
  ungroup()

# fill music instrument by down
df_combined <- df_combined %>% 
  group_by(id) %>% 
  fill(music, .direction = "down")

# check discrepancies
discrepancies <- df_combined %>%
  group_by(id) %>%
  filter(n_distinct(music, na.rm = TRUE) > 1)

# change to yes's if at least once responded as yes
df_combined <- df_combined %>% 
  group_by(id) %>% 
  mutate(music = ifelse("Yes" %in% music, "Yes", music)) %>%
  ungroup()

# fill ancestry by down
#df_combined <- df_combined %>% 
#  group_by(id) %>% 
#  fill(ancestry, .direction = "down")

# check discrepancies
#discrepancies <- df_combined %>%
#  group_by(id) %>%
#  filter(n_distinct(ancestry, na.rm = TRUE) > 1)

# too few observations (228) -- drop
#df_combined <- df_combined %>% 
#  select(-ancestry, -ancestry_other)

# fill education by down
#df_combined <- df_combined %>% 
#  group_by(id) %>% 
#  fill(education, .direction = "down")

## too few observations (301) -- drop
#df_combined <- df_combined %>% 
#  select(-education)

# fill used by down
df_combined <- df_combined %>% 
  group_by(id) %>% 
  fill(used, .direction = "down")

# check discrepancies
discrepancies <- df_combined %>%
  group_by(id) %>%
  filter(n_distinct(used, na.rm = TRUE) > 1)

discrepancies <- discrepancies %>%
  select(id, used, everything())

# change to yes's if at least once responded as yes
df_combined <- df_combined %>% 
  group_by(id) %>% 
  mutate(used = ifelse("Yes" %in% used, "Yes", used)) %>%
  ungroup()

df_combined$cannabis_freqnum <- 0

freqlist <-    c('Not in the past 3 months',
                 'Once or twice within these past 3 months',
                 'Around once a month',
                 'A few times a month',
                 'Once a week',
                 'A few times a week',
                 'Daily')

for (idx in c(1:length(freqlist))) {
  df_combined$cannabis_freqnum[which(df_combined$use_frequency == freqlist[idx])] <- idx
}

df_combined <- df_combined %>%
  mutate(cannabis_freqnum = if_else(is.na(use_frequency), NA_real_, cannabis_freqnum))

df_combined <- df_combined %>%
  select(id, used, use_frequency, cannabis_freqnum, everything())

# fill used by down
df_combined <- df_combined %>% 
  group_by(id) %>% 
  fill(cannabis_freqnum, .direction = "down")

# check discrepancies
discrepancies <- df_combined %>%
  group_by(id) %>%
  filter(n_distinct(cannabis_freqnum, na.rm = TRUE) > 1)

# fill in cannabis_freqnum by max
df_combined <- df_combined %>%
  group_by(id) %>%
  mutate(cannabis_freqnum = if(all(is.na(cannabis_freqnum))) NA else max(cannabis_freqnum, na.rm = TRUE)) %>%
  ungroup()

# calculate cannabis groups

df_combined <- df_combined %>% mutate(cannabis_group = cannabis_freqnum,
                    cannabis_group = case_when(cannabis_freqnum == 0 ~ "Non-users",
                                               cannabis_freqnum > 5 ~ "Frequent users",
                                               TRUE ~ "Infrequent users"))
df_combined$cannabis_group <- factor(df_combined$cannabis_group, ordered = FALSE)
df_combined$cannabis_group <- relevel(df_combined$cannabis_group, ref = "Non-users")

df_combined <- df_combined %>% 
  select(-used, -use_frequency)

# remove duplicated 
double_info <- df_combined[duplicated(df_combined[c("id", "date_date")], fromLast = TRUE), ]

df_combined <- df_combined[!duplicated(df_combined[c("id", "date_date")], fromLast = TRUE), ]

write.csv(df_combined, file.path("data", "output", "full_questionnaire.csv"), row.names = FALSE)

# Perform linear regression
model <- lm(stressed ~ cannabis_group, data = df_combined)

options(scipen=999)

# View summary of regression model
summary(model)

# Perform linear regression
model <- lm(physically_activity ~ cannabis_group + factor(id), data = df_combined)

# View summary of regression model
summary(model)



