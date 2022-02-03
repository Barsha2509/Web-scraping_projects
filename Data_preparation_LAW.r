# Title: PH1976 Final Project
# Author: Cuihong Zhang
# Date: 3/13/2020

# read the data
path <- "/Users/abhisheksinha/Desktop/Project/"  ## please change the direction## please change the direction
demo_train <- read.csv(paste0(path,"demo_train.csv"), stringsAsFactors = FALSE)
demo_test <- read.csv(paste0(path,"demo_test.csv"), stringsAsFactors = FALSE)
meds_train <- read.csv(paste0(path,"medication_train.csv"), stringsAsFactors = FALSE)
meds_test <- read.csv(paste0(path,"medication_test.csv"), stringsAsFactors = FALSE)
procs_train <- read.csv(paste0(path,"procedure_train.csv"), stringsAsFactors = FALSE)
procs_test <- read.csv(paste0(path,"procedure_test.csv"), stringsAsFactors = FALSE)

# merge train and test together, in order to clean and organzie the train and test simutaneously
demo_test$death <- NA
demo_total <- rbind(demo_train, demo_test)
meds_total <- rbind(meds_train, meds_test)
procs_total <- rbind(procs_train, procs_test)
demo_total$los <- as.numeric(difftime(as.POSIXlt(demo_total$New_discharge_dt_tm, format='%Y-%m-%d %H:%M:%S'), as.POSIXlt(demo_total$New_admitted_dt_tm, format='%Y-%m-%d %H:%M:%S'), tz="UTC", unit = "days"))

n.med <- length(unique(meds_total$generic_name))  ## check the number of unique meds
n.proc <- length(unique(procs_total$procedure_id))  ## check the number of unique procs
library(plyr)
procs_count = count(procs_total, "procedure_description")
meds_count = count(meds_total, "generic_name")

# transfer data from long format to wide format, and matched up the data by "patient_sk"
library(reshape2)
med.wide <- dcast(meds_total, patient_sk ~ generic_name)
proc.wide <- dcast(procs_total, patient_sk ~ procedure_id)
total <- merge(demo_total, proc.wide, by="patient_sk")
total <- merge(total, med.wide, by="patient_sk")

total$vasopressors <- 0 
total$vasopressors[total$dopamine >= 1 | total$phenylephrine >= 1 | total$norepinephrine >= 1 | total$'phenylephrine nasal'>= 1 | total$'phenylephrine ophthalmic' >= 1 | total$vasopressin >= 1 | total$epinephrine >= 1 | total$'oxymetazoline nasal' >= 1 | total$dobutamine >= 1] <- 1
total$vasopressors = as.factor(total$vasopressors)

total$antifibrinolytic_agents <- 0
total$antifibrinolytic_agents[total$'aminocaproic acid'>= 1 | total$'aprotinin' >= 1 | total$'fibrinolysis inhibitor-thrombin topical'>= 1 | total$'tranexamic acid'>=1 ] <- 1
total$antifibrinolytic_agents= as.factor(total$antifibrinolytic_agents)


total$analgesics <- 0
total$analgesics[total$'acetaminophen'>= 1 | total$'acetaminophen-codeine' >= 1 | total$'acetaminophen-oxycodone'>= 1 | total$'acetaminophen-propoxyphene'>=1 | total$'acetaminophen-tramadol' >= 1 | total$'acetaminophen/butalbital/caffeine'>= 1 | total$'asa/butalbital/caffeine'>= 1 | total$'apap/butalbital/caffeine'>= 1 | total$'alfentanil'>=1 | total$'antipyrine-benzocaine otic' >= 1 | total$'belladonna-opium'| total$'codeine'>= 1 | total$'diclofenac'>= 1 | total$'dihydroergotamine'>= 1 |total$'fentanyl'>=1 | total$'hydrocodone-ibuprofen' >= 1 | total$'hydromorphone'>= 1 | total$'indomethacin'>=1 | total$'ketorolac' >= 1 | total$'ketorolac ophthalmic' >= 1 |total$'butorphanol'>=1 | total$'meperidine'>=1 | total$'methadone' >= 1 | total$'morphine'>= 1 | total$'nabumetone'>= 1 | total$'nalbuphine'>=1| total$'naloxone' >= 1 | total$'naloxone-pentazocine'>= 1 | total$'naproxen'>=1 | total$'oxycodone' >= 1 | total$'pentazocine'>= 1 | total$'phenazopyridine'>=1 | total$'pregabalin'>= 1 | total$'propoxyphene'>= 1 | total$'pentafluoropropane-tetrafluoroethane topical'>= 1 | total$'remifentanil'>=1 | total$'sufentanil' >= 1 | total$'sulindac'>= 1 | total$'tramadol'>=1| total$'ibuprofen'>=1 | total$'trolamine salicylate topical'>=1 | total$methylergonovine | total$propoxyphene>= 1 | total$dihydroergotamine >= 1] <- 1
total$analgesics= as.factor(total$analgesics)


total$deficiencymed <- 0
total$deficiencymed[total$'zinc sulfate'>= 1 | total$"vitamins a, d, and e topical" >= 1 | total$"vitamin e topical" >= 1 | total$"vitamin e" >=1 | total$'thiamine' >= 1 | total$'selenium'>= 1 | total$'potassium phosphate' >= 1 | total$"potassium phosphate-sodium phosphate" >= 1 | total$'potassium chloride' >=1 | total$'potassium bicarbonate-potassium citrate' >= 1 | total$'potassium acid phosphate'>= 1  | total$'phytonadione'>= 1 | total$'paricalcitol' >= 1 | total$'multivitamin'>= 1 |total$"multivitamin, prenatal" >=1 | total$'multivitamin with minerals'  >= 1 | total$'multivitamin with iron' >= 1 | total$'niacin' >=1 | total$'iron sucrose' >= 1 |total$'magnesium chloride'>=1 | total$'magnesium citrate'>=1 | total$'magnesium hydroxide'>= 1 | total$'magnesium oxide' >= 1 | total$'magnesium sulfate' >= 1 | total$'immune globulin intravenous' >=1| total$'glucose' >= 1 | total$'glucagon'>= 1 | total$'folic acid' >=1 | total$'ferrous sulfate'>= 1 | total$"fat emulsion, intravenous">= 1 | total$'ergocalciferol' >=1 | total$'epoetin alfa'>= 1 | total$'doxercalciferol' >= 1 | total$'cyanocobalamin'>= 1 | total$'citric acid-potassium bicarbonate'>=1 | total$'cholecalciferol' >= 1 | total$'calcium-vitamin d' >= 1 | total$'calcium acetate' >=1| total$calcitriol>=1 | total$'ascorbic acid'>=1 | total$'calcium and vitamin d combination' >= 1 | total$'calcium carbonate' >= 1 | total$'calcium chloride'>= 1 | total$'calcium gluconate' >= 1 | total$'ubiquinone' >= 1] <- 1
total$deficiencymed= as.factor(total$deficiencymed)

total$vasodilators <- 0
total$vasodilators[total$nitroglycerin >= 1 | total$nitroprusside >= 1 | total$cilostazol >= 1 | total$hydralazine >= 1 | total$pentoxifylline >= 1 | total$sildenafil >= 1]<-1
total$vasodilators = as.factor(total$vasodilators)

 total$respiratorymed <- 0
total$respiratorymed[total$zafirlukast >= 1 | total$theophylline >= 1 | total$salmeterol >= 1 | total$metaproterenol >= 1 | total$montelukast >= 1 | total$ipratropium >= 1 | total$levalbuterol>=1 | total$tiotropium>=1 | total$albuterol >= 1 | total$'albuterol-ipratropium' >= 1 | total$formoterol >= 1 | total$flunisolide >= 1 | total$budesonide >= 1 | total$beclomethasone >= 1 | total$'mometasone nasal'>= 1 | total$aminophylline >= 1 | total$'budesonide-formoterol' >= 1 | total$'beclomethasone nasal' >= 1 | total$'flunisolide nasal'>=1 | total$'fluticasone-vilanterol' >= 1 | total$'fluticasone'>=1 | total$'fluticasone-vilanterol' >= 1 |total$'ipratropium nasal' >= 1 | total$'fluticasone-salmeterol' >= 1 | total$'acetylcysteine' >= 1 | total$doxapram >= 1 | total$arformoterol >= 1 | total$ephedrine >= 1]<-1
total$respiratorymed = as.factor(total$respiratorymed)

total$diuretic <- 0
total$diuretic[total$triamterene >= 1 | total$torsemide >= 1 | total$spironolactone >= 1 | total$metolazone >= 1 | total$mannitol >= 1 | total$indapamide >= 1 | total$hydrochlorothiazide>=1 | total$furosemide >= 1 | total$chlorothiazide >= 1 | total$bumetanide >= 1 | total$beclomethasone >= 1 | total$aminophylline >= 1 | total$'amiloride' >= 1 | total$'acetazolamide' >= 1 | total$'hydrochlorothiazide-triamterene'>=1]<-1
total$diuretic = as.factor(total$diuretic)

 total$bloodthinners <- 0
total$bloodthinners[total$warfarin >= 1 | total$tirofiban>= 1 | total$ticagrelor >= 1 | total$rivaroxaban >= 1 | total$prasugrel >= 1 | total$heparin >= 1 | total$fondaparinux>=1 | total$eptifibatide >= 1 | total$dipyridamole >= 1 | total$dalteparin >= 1 | total$dabigatran >= 1 | total$bivalirudin >= 1 | total$'argatroban' >= 1 | total$'heparin flush' >= 1 | total$'anagrelide' >= 1 | total$'abciximab'>=1]<-1
total$bloodthinners = as.factor(total$bloodthinners)

total$antihypertensives <- 0
total$antihypertensives[total$amiloride >=1 | total$amlodipine >= 1 | total$`atenolol` >= 1 | total$`aliskiren` >= 1 | total$candesartan  >=1 | total$`captopril` >= 1 | total$`bisoprolol` >= 1 | total$'bisoprolol-hydrochlorothiazide' >= 1 |  total$`bumetanide` >= 1 | total$`carvedilol` >= 1 | total$'clevidipine' >= 1 | total$`chlorothiazide` >= 1 | total$`diltiazem` >= 1 | total$`enalapril` >= 1| total$esmolol  >=1 | total$`felodipine` >= 1 | total$`fenoldopam` >= 1 |total$`fosinopril` >= 1 | total$`indapamide` >= 1 | total$irbesartan  >= 1 | total$`isradipine` >= 1 | total$labetalol  >=1 | total$`lisinopril` >= 1 | total$`losartan` >= 1 | total$`metoprolol` >= 1 | total$moexipril >= 1| total$methyldopa >= 1 | total$`nadolol` >= 1 | total$nebivolol  >=1 | total$`nicardipine` >= 1 | total$`nifedipine` >= 1 | total$`nimodipine` >= 1| total$nitroprusside >=1 | total$olmesartan >=1 | total$`pindolol` >= 1 | total$`propranolol` >= 1 | total$`verapamil` >= 1 | total$valsartan >= 1 | total$`telmisartan`>= 1 | total$`ramipril` >= 1 | total$`quinapril`>= 1 | total$`amlodipine-benazepril`>= 1 | total$`methyldopa`>= 1 | total$`phentolamine`>= 1 | total$`hydrochlorothiazide-valsartan`>= 1 | total$`hydrochlorothiazide-losartan`>= 1 | total$`hydrochlorothiazide-lisinopril`>= 1 | total$'timolol ophthalmic'>= 1] <-1
total$antihypertensives = as.factor(total$antihypertensives)

total$dermalmed <- 0
total$dermalmed[total$'allantoin/camphor/phenol topical'>=1 | total$'ammonium lactate topical' >= 1 | total$`balsam peru-castor oil topical` >= 1 | total$`balsam peru/castor oil/trypsin topical` >= 1 | total$"emollients, topical">=1 | total$`glycerin` >= 1 | total$`collagenase topical` >= 1 | total$'glycerin-witch hazel topical' >= 1 |  total$`mineral oil` >= 1 | total$`mometasone` >= 1 | total$'petrolatum topical' >= 1 | total$`phenol topical` >= 1 | total$`pramoxine-zinc oxide topical` >= 1 | total$`resorcinol topical` >= 1| total$tretinoin>=1 | total$`triamcinolone` >= 1 | total$`triamcinolone topical` >= 1 | total$`urea topical` >= 1 | total$`zinc oxide topical` >= 1] <-1
total$dermalmed = as.factor(total$dermalmed)

total$antidiabetic <- 0
total$antidiabetic[total$acarbose >=1 | total$desmopressin >= 1 | total$glimepiride >= 1 | total$`glipizide` >= 1 | total$`glyburide` >= 1 | total$'glyburide-metformin'  >=1 | total$`captopril` >= 1 | total$`bisoprolol` >= 1 | total$'insulin' >= 1 |  total$`insulin aspart` >= 1 | total$`insulin aspart-insulin aspart protamine` >= 1 | total$'insulin detemir' >= 1 | total$`insulin glargine` >= 1 | total$`insulin glulisine` >= 1 | total$`insulin isophane` >= 1 | total$'insulin isophane-insulin regular'  >=1 | total$`insulin lispro` >= 1 | total$`insulin regular` >= 1 |total$`linagliptin` >= 1 | total$`metformin` >= 1 | total$nateglinide >= 1 | total$`pioglitazone` >= 1 | total$`repaglinide` >= 1 | total$`rosiglitazone` >= 1 | total$sitagliptin >= 1| total$vasopressin >= 1] <-1
total$antidiabetic = as.factor(total$antidiabetic)

total$sedatives <- 0
total$sedatives[total$alprazolam >=1 | total$amobarbital >= 1 | total$`butabarbital` >= 1 | total$`clonazepam` >= 1 | total$'clobazam'  >=1 | total$`captopril` >= 1 | total$`bisoprolol` >= 1 | total$'clonidine' >= 1 |  total$`clorazepate` >= 1 | total$`dexmedetomidine` >= 1 | total$'diazepam' >= 1 | total$`ethchlorvynol` >= 1| total$`lorazepam` >= 1 | total$methylphenidate >= 1 | total$`oxazepam` >= 1 | total$'pentobarbital'  >=1 | total$`phenobarbital` >= 1 | total$`temazepam` >= 1 | total$`thiopental` >= 1 | total$`trazodone` >= 1 | total$zolpidem >= 1 | total$modafinil >= 1] <-1
total$sedatives = as.factor(total$sedatives)

total$anesthetics <- 0
total$anesthetics[total$bupivacaine >=1 | total$desflurane >= 1 | total$`lidocaine` >= 1 | total$`methohexital` >= 1 | total$'propofol'>=1 | total$`ropivacaine` >= 1 | total$`tetracaine ophthalmic` >= 1 | total$'proparacaine ophthalmic' >= 1 |  total$`bupivacaine-epinephrine` >= 1 | total$`benzocaine topical` >= 1 | total$'benzocaine-tetracaine topical' >= 1 | total$`bupivacaine-epinephrine`>= 1| total$`etomidate` >= 1 | total$ketamine >= 1 | total$`lidocaine-prilocaine topical` >= 1 | total$'lidocaine-sodium bicarbonate'  >=1 | total$`lidocaine topical` >= 1 ] <-1
total$anesthetics = as.factor(total$anesthetics)

total$antibiotics <- 0
total$antibiotics[total$`azithromycin`>=1 | total$'amoxicillin' >= 1 | total$'amoxicillin-clavulanate' >= 1 | total$`ampicillin` >= 1 | total$'ampicillin-sulbactam' >=1 | total$`azithromycin ophthalmic` >= 1 |  total$`aztreonam` >= 1 | total$`bacitracin` >= 1 | total$'bacitracin ophthalmic' >= 1 | total$`bacitracin topical` >= 1 | total$`bacitracin-polymyxin b ophthalmic` >= 1 | total$`bacitracin-polymyxin b topical` >= 1 | total$`bacitracin/neomycin/polymyxin b topical`>=1 | total$`cefadroxil` >= 1 | total$`cefazolin` >= 1 | total$`cefotaxime` >= 1 | total$`cefotetan` >= 1 | total$cefoxitin  >= 1 | total$`cefpodoxime` >= 1 | total$tobramycin  >=1 | total$`sulfamethoxazole-trimethoprim` >= 1 | total$`tobramycin ophthalmic` >= 1 | total$`ceftazidime` >= 1 | total$ceftriaxone >= 1 | total$`cefuroxime` >= 1 | total$celecoxib >=1 | total$`chloramphenicol` >= 1 | total$`ciprofloxacin` >= 1 | total$'ciprofloxacin-dexamethasone otic' >= 1 | total$clarithromycin >=1 | total$clindamycin >=1 | total$`dapsone` >= 1 | total$`daptomycin` >= 1 | total$`diclofenac` >= 1 | total$doripenem >= 1 | total$`doxycycline`>= 1| total$`ertapenem` >= 1 | total$`erythromycin`>= 1| total$`erythromycin ophthalmic`>= 1 | total$`gatifloxacin`>= 1 | total$`gatifloxacin ophthalmic`>= 1 | total$`gentamicin`>= 1 | total$`gentamicin ophthalmic`>= 1 | total$`gramicidin/neomycin/polymyxin b ophthalmic` >= 1 | total$cephalexin >=1 | total$`isoniazid` >= 1 | total$`levofloxacin` >= 1 | total$`linezolid` >= 1 | total$metronidazole >=1 | total$'metronidazole topical'>=1 | total$`minocycline` >= 1 | total$`moxifloxacin` >= 1 | total$`moxifloxacin ophthalmic` >= 1 | total$nafcillin >= 1 | total$`neomycin`>= 1| total$`neomycin-polymyxin b sulfate topical` >= 1 | total$`ofloxacin`>= 1 | total$`oxacillin`>= 1 | total$`penicillin`>= 1 | total$`piperacillin-tazobactam`>= 1 | total$`polymyxin b sulfate`>= 1 | total$`rifampin`>= 1| total$rifaximin>= 1 | total$`silver sulfadiazine topical`>= 1 | total$`imipenem-cilastatin`>= 1 | total$'dexamethasone-tobramycin ophthalmic' | total$'dexamethasone/neomycin/polymyxin b ophthalmic' | total$`indomethacin`>= 1 | total$`imipenem-cilastatin`>= 1] <-1
total$antibiotics = as.factor(total$antibiotics)

total$urinary_retention<- 0
total$urinary_retention[total$`alfuzosin`>=1 | total$doxazosin >= 1 | total$`dutasteride` >= 1| total$finasteride >=1 | total$`prazosin` >= 1 | total$`tamsulosin` >= 1 | total$`terazosin` >= 1] <-1
total$urinary_retention= as.factor(total$urinary_retention)

total$antipsychotic <- 0
total$antipsychotic[total$`aripiprazole`>=1 | total$chlorpromazine >= 1 | total$`droperidol` >= 1 | total$haloperidol >=1 | total$`lithium` >= 1 | total$`loxapine` >= 1 | total$`midazolam` >= 1 | total$olanzapine >= 1 | total$`prochlorperazine` >= 1| total$quetiapine >=1 | total$`risperidone` >= 1 | total$`ziprasidone` >= 1] <-1
total$antipsychotic= as.factor(total$antipsychotic)

total$glaucoma_med <- 0
total$glaucoma_med[total$`acetazolamide`>=1 | total$`physostigmine`>=1 | total$'bimatoprost ophthalmic' >= 1 | total$`brimonidine ophthalmic` >= 1 | total$'dorzolamide ophthalmic' >=1 | total$`dorzolamide-timolol ophthalmic` >= 1 | total$`latanoprost ophthalmic` >= 1 | total$`levobunolol ophthalmic` >= 1 | total$'pilocarpine ophthalmic' >= 1 | total$`travoprost ophthalmic` >= 1] <-1
total$glaucoma_med= as.factor(total$glaucoma_med)

total$antiviral <- 0
total$antiviral[total$`abacavir/dolutegravir/lamivudine`>=1 | total$'acyclovir' >= 1 | total$`amantadine` >= 1 | total$'atazanavir' >=1 | total$`dorzolamide-timolol ophthalmic` >= 1 | total$`latanoprost ophthalmic` >= 1 | total$`levobunolol ophthalmic` >= 1 | total$'pilocarpine ophthalmic' >= 1 | total$`docosanol topical` >= 1 | total$'hepatitis b vaccine' >= 1 | total$`hepatitis b adult vaccine` >= 1 | total$`influenza virus vaccine`>=1 | total$"influenza virus vaccine, inactivated" >= 1 | total$`lopinavir-ritonavir` >= 1 | total$'lamivudine-zidovudine' >=1 | total$`lamivudine` >= 1 | total$`oseltamivir` >= 1 | total$`ritonavir` >= 1 | total$`valacyclovir` >= 1 | total$'valganciclovir' >= 1 ] <-1
total$antiviral= as.factor(total$antiviral)

total$antifungal <- 0
total$antifungal[total$`clotrimazole`>=1 | total$'clotrimazole topical' >= 1 | total$`fluconazole` >= 1 | total$'caspofungin' >=1 | total$`atovaquone` >= 1 | total$`amphotericin b` >= 1 | total$'amphotericin b liposomal' >= 1] <-1
total$antifungal= as.factor(total$antifungal)

 total$thrombolytic <- 0
total$thrombolytic[total$`alteplase`>=1 | total$'apixaban' >= 1 | total$`cangrelor` >= 1 | total$'enoxaparin' >=1 | total$`tenecteplase` >= 1 | total$'reteplase' >= 1 ] <-1
total$thrombolytic= as.factor(total$thrombolytic)

total$antiarrhythmic <- 0
total$antiarrhythmic[total$`adenosine`>=1 | total$'amiodarone' >= 1 | total$`digoxin` >= 1 | total$'flecainide' >=1 | total$`mexiletine` >= 1 | total$`procainamide` >= 1 | total$`propafenone` >= 1 | total$`dofetilide` >= 1] <-1
total$antiarrhythmic= as.factor(total$antiarrhythmic)

total$anticonvulsant <- 0
total$anticonvulsant[total$`zonisamide`>=1 | total$'valproic acid' >= 1 | total$`topiramate` >= 1 | total$'phenobarbital' >=1 | total$`oxcarbazepine` >= 1 | total$`lacosamide` >= 1 | total$`lamotrigine` >= 1 | total$'levetiracetam' >= 1 | total$`gabapentin` >= 1 | total$`fosphenytoin`>=1 | total$carbamazepine >= 1 | total$`divalproex sodium` >= 1] <-1
total$anticonvulsant= as.factor(total$anticonvulsant)

total$antacid <- 0
total$antacid[total$`sucralfate`>=1 | total$'ranitidine' >= 1 | total$`famotidine` >= 1 | total$'nizatidine' >=1 | total$`cimetidine` >= 1 | total$`al hydroxide/mg hydroxide/simethicone` >= 1 | total$`aluminum hydroxide-magnesium hydroxide` >= 1 | total$'al hydroxide/asa/ca carbonate/mg hydroxide' >= 1 ] <-1
total$antacid= as.factor(total$antacid)

total$antihistamine <- 0
total$antihistamine[total$`cetirizine`>=1 | total$'chlorpheniramine' >= 1 | total$`diphenhydramine` >= 1 | total$'fexofenadine' >=1 | total$`hydroxyzine` >= 1 | total$`loratadine` >= 1 | total$`meclizine` >= 1 | total$'promethazine' >= 1 | total$`pseudoephedrine` >= 1 | total$'fexofenadine-pseudoephedrine' >=1 | total$`diphenhydramine topical` >= 1 | total$`chlorpheniramine-hydrocodone` >= 1 | total$`ketotifen ophthalmic` >= 1 ] <-1
total$antihistamine= as.factor(total$antihistamine)

total$cholestrolmed <- 0
total$cholestrolmed[total$`ursodiol`>=1 | total$'simvastatin' >= 1 | total$`rosuvastatin` >= 1 | total$'pravastatin' >=1 | total$`lovastatin` >= 1 | total$`gemfibrozil` >= 1 | total$`fluvastatin` >= 1 | total$`omega-3 polyunsaturated fatty acids` >= 1 | total$'fenofibrate' >= 1 | total$`ezetimibe` >= 1 | total$`ezetimibe-simvastatin`>=1 | total$colesevelam >= 1 | total$`colestipol` >= 1 | total$`clopidogrel` >= 1 | total$`factor ix complex` >= 1 | total$`cholestyramine`>=1 | total$atorvastatin >= 1] <-1
total$cholestrolmed= as.factor(total$cholestrolmed)

total$steroid <- 0
total$steroid[total$prednisone>=1 | total$dexamethasone >= 1 | total$betamethasone >= 1 | total$methylprednisolone >=1 | total$fludrocortisone >= 1 | total$hydrocortisone >= 1 | total$`dexamethasone ophthalmic` >= 1 | total$`prednisolone-sodium sulfacetamide ophthalmic` >= 1 | total$'prednisolone ophthalmic' >= 1 | total$`fluticasone nasal` >= 1 | total$`cosyntropin` >= 1 |total$`hydrocortisone topical`>=1] <-1
total$steroid= as.factor(total$steroid)

total$muscle_relaxant <- 0
total$muscle_relaxant[total$vecuronium >= 1 | total$rapacuronium >= 1 | total$cyclobenzaprine >=1 | total$carisoprodol >= 1 | total$cisatracurium >= 1 | total$`atracurium` >= 1 | total$`baclofen` >= 1 | total$succinylcholine >= 1 | total$pancuronium >= 1 | total$`metaxalone`>=1 | total$methocarbamol >= 1 | total$mivacurium>=1 | total$tizanidine>=1] <-1
total$muscle_relaxant= as.factor(total$muscle_relaxant)

total$arthritismed <- 0
total$arthritismed[total$colchicine >= 1 | total$azathioprine >= 1 | total$allopurinol >=1 | total$sulfasalazine >= 1 | total$probenecid >= 1 | total$meloxicam >= 1 | total$leflunomide >= 1 | total$glucosamine >= 1 | total$'methyl salicylate topical' >= 1 | total$`menthol topical`>=1 | total$'trolamine salicylate topical' >= 1 | total$'capsaicin topical'>=1] <-1
total$arthritismed= as.factor(total$arthritismed)

 total$depression_med <- 0
total$depression_med[total$venlafaxine >= 1 | total$desipramine >= 1 | total$desvenlafaxine >=1 | total$doxepin >= 1 | total$duloxetine >= 1 | total$bupropion >= 1 | total$citalopram >= 1 | total$amitriptyline >= 1 | total$sertraline>= 1 | total$nortriptyline>=1 | total$paroxetine >= 1 | total$mirtazapine>=1 | total$nefazodone >= 1 | total$ escitalopram>= 1 | total$fluoxetine >= 1 | total$fluvoxamine >= 1 | total$imipramine>= 1] <-1
total$depression_med= as.factor(total$depression_med)

total$cardiovascular <- 0
total$cardiovascular[total$dobutamine>=1 | total$dopamine>= 1 | total$ranolazine >= 1 | total$midodrine >=1 | total$isoproterenol >= 1 | total$'isosorbide dinitrate'>= 1 | total$'isosorbide mononitrate' >= 1 | total$`hydralazine-isosorbide dinitrate` >= 1] <-1
total$cardiovascular= as.factor(total$cardiovascular)

total$antiemetic <- 0
total$antiemetic[total$trimethobenzamide>=1 | total$dimenhydrinate >= 1 | total$dolasetron >= 1 | total$droperidol >=1 | total$aprepitant >= 1 | total$scopolamine >= 1 | total$granisetron >= 1 | total$`benzocaine-cetylpyridinium topical` >= 1 | total$metoclopramide >= 1] <-1
total$antiemetic= as.factor(total$antiemetic)

total$antiseptic <- 0
total$antiseptic[total$ivermectin>=1 | total$'silver nitrate topical' >= 1 | total$'povidone iodine topical' >= 1 | total$'oxychlorosene sodium topical'>=1 | total$'hydrogen peroxide topical' >= 1 | total$'chlorhexidine topical'>= 1] <-1
total$antiseptic= as.factor(total$antiseptic)

total$contrast_medium <- 0
total$contrast_medium[total$diatrizoate>=1 | total$perflutren >= 1 | total$iohexol >= 1 | total$iopamidol>=1 | total$ioversol >= 1 | total$gadobutrol>= 1 | total$gadodiamide>=1 | total$gadoteridol >= 1 | total$gadoversetamide >= 1 | total$'barium sulfate'>=1 | total$iodixanol >= 1 | total$'gadopentetate dimeglumine' >= 1 | total$'gadobenate dimeglumine'>=1 | total$'indocyanine green' >= 1] <-1
total$contrast_medium= as.factor(total$contrast_medium)

total$hormone <- 0
total$hormone[total$estradiol>=1 | total$estropipate>= 1 | total$testosterone >= 1 | total$octreotide>=1 | total$megestrol >= 1 | total$medroxyprogesterone >=1 | total$'conjugated estrogens' >= 1] <-1
total$hormone= as.factor(total$hormone)

total$parkinsonmed<- 0
total$parkinsonmed[total$ropinirole >= 1 | total$pramipexole >= 1 | total$benztropine >= 1 | total$entacapone >= 1 | total$'carbidopa-levodopa'>= 1 | total$trihexyphenidyl>= 1]<-1
total$parkinsonmed = as.factor(total$parkinsonmed)

total$obstetric<- 0
total$obstetric[total$carboprost >= 1 | total$oxytocin >= 1]<-1
total$obstetric = as.factor(total$obstetric)

total$bone_marrow_stimulant<- 0
total$bone_marrow_stimulant[total$filgrastim >= 1 | total$pegfilgrastim >= 1 | total$'darbepoetin alfa' >= 1]<-1
total$bone_marrow_stimulant = as.factor(total$bone_marrow_stimulant)

total$bone_health<- 0
total$bone_health[total$alendronate >= 1 | total$raloxifene >= 1]<-1
total$bone_health = as.factor(total$bone_health)

 total$thyroidmed<- 0
total$thyroidmed[total$propylthiouracil >= 1 | total$methimazole >= 1 | total$liothyronine >= 1 | total$levothyroxine >= 1 | total$'thyroid desiccated'>= 1]<-1
total$thyroidmed = as.factor(total$thyroidmed)

 total$immunosuppressive<- 0
total$immunosuppressive[total$tacrolimus >= 1 | total$'mycophenolate mofetil' >= 1 | total$'mycophenolic acid' >= 1]<-1
total$immunosuppressive = as.factor(total$immunosuppressive)

total$chemotherapy<- 0
total$chemotherapy[total$cytarabine >= 1 | total$bicalutamide >= 1 | total$anastrozole >= 1 | total$tamoxifen >= 1 | total$pamidronate>= 1 | total$melphalan>= 1 | total$methotrexate>= 1 | total$hydroxyurea>= 1]<-1
total$chemotherapy = as.factor(total$chemotherapy)

total$coughmed<- 0
total$coughmed[total$benzonatate>= 1 | total$guaifenesin >= 1 | total$'menthol-phenol topical' >= 1 | total$'codeine-guaifenesin'>= 1 | total$'dextromethorphan-guaifenesin'>= 1]<-1
total$coughmed = as.factor(total$coughmed)

total$pupildilator<- 0
total$pupildilator[total$atropine >= 1 | total$'atropine ophthalmic'>= 1 | total$'tropicamide ophthalmic' >= 1 | total$'cyclopentolate ophthalmic'>= 1]<-1
total$pupildilator = as.factor(total$pupildilator)

total$dementiamed<- 0
total$dementiamed[total$rivastigmine >= 1 | total$donepezil >= 1 | total$memantine >= 1 | total$galantamine >= 1 | total$hyoscyamine>= 1]<-1
total$dementiamed = as.factor(total$dementiamed)

total$gastrointestinal<- 0
total$gastrointestinal[total$`psyllium`>=1 | total$`docusate-senna`>=1 | total$bisacodyl >= 1 | total$`sodium biphosphate-sodium phosphate` >= 1 | total$lactulose >= 1 | total$senna >=1 | total$lubiprostone >= 1 | total$dicyclomine >= 1 |total$`methylnaltrexone` >= 1 | total$`atropine-diphenoxylate` >= 1 | total$`atropine/hyoscyamine/pb/scopolamine` >= 1 | total$`bifidobacterium infantis` >= 1 | total$alvimopan >= 1 | total$'saccharomyces boulardii lyo'>= 1 | total$simethicone>= 1 | total$'polyethylene glycol 3350' >= 1 | total$'polyethylene glycol 3350 with electrolytes'>= 1 | total$'polyethylene glycol electrolyte solution'>= 1 | total$omeprazole>= 1 | total$pantoprazole>= 1 | total$'lactobacillus acidophilus and bulgaricus'>= 1 | total$'lactobacillus rhamnosus gg'>= 1 | total$lansoprazole>= 1 | total$loperamide>= 1 | total$lubiprostone>= 1 | total$glycopyrrolate>= 1 | total$esomeprazole>= 1 | total$'sodium bicarbonate'>= 1 | total$sorbitol >= 1 | total$`sodium phosphate` >= 1 | total$`guar gum` >= 1] <-1
total$gastrointestinal= as.factor(total$gastrointestinal)

 total$clot_agents<- 0
total$clot_agents[total$`anti-inhibitor coagulant complex`>=1 | total$`antihemophilic factor`>=1 |total$desmopressin >= 1 | total$`coagulation factor viia` >= 1 | total$`prothrombin complex` >= 1 | total$'thrombin topical' >= 1 | total$protamine >=1 | total$`159797` >= 1 | total$`3932` >= 1] <-1
total$clot_agents = as.factor(total$clot_agents)

total$vaccine <- 0
total$vaccine[total$`pneumococcal 13-valent vaccine`>=1 | total$'pneumococcal 23-polyvalent vaccine'>= 1 | total$'pneumococcal vaccine' >= 1 | total$"tetanus/diphtheria/pertussis, acel (tdap)">= 1 | total$'tetanus toxoid'>= 1 | total$`tetanus-diphth toxoids (td) adult/adol` >= 1 | total$`tetanus-diphtheria toxoids` >= 1 | total$`tetanus/diphth/pertuss (tdap) adult/adol` >= 1 | total$`diphtheria-tetanus toxoids` >= 1 | total$"diphtheria/pertussis, acellular/tetanus" >= 1] <- 1
total$vaccine = as.factor(total$vaccine)

total$euvolemia <- 0
total$euvolemia[total$`plasma protein fraction`>=1 | total$"dextran, low molecular weight" >= 1 | total$'albumin human' >= 1 | total$'sodium chloride'>= 1 | total$'immune globulin intravenous'>= 1 | total$`356` >= 1 | total$'lvp solution'>= 1 | total$'lvp solution with hypertonic saline'>= 1 | total$'lvp solution with potassium'>= 1 | total$`356` >= 1 | total$'lvp solution'>= 1 | total$`hetastarch` >= 1 | total$`310` >= 1 | total$`2610` >= 1 | total$`155972` >= 1 | total$`155524` >= 1 | total$`155966` >= 1 | total$`155520` >= 1 | total$`3931` >= 1 | total$`9045` >= 1 | total$`155522` >= 1 | total$`156911` >= 1] <- 1
total$euvolemia = as.factor(total$euvolemia)

total$resp_sup <-0
total$resp_sup[total$`141` >= 1 | total$`8165` >= 1 | total$`122879` >= 1 | total$`123327` >= 1 |total$`122875` >= 1 | total$`2548` >= 1 | total$`2549` >= 1 | total$`154650` >= 1 | total$`161794` >= 1 | total$`161793` >= 1 | total$`2547` >= 1 | total$`2479` >= 1 | total$`15200` >= 1 | total$`161771` >= 1 | total$`161781` >= 1 | total$`161776` >= 1 | total$`161772` >= 1]<-1
total$resp_sup = as.factor(total$resp_sup)

total$txt_ans <- 0
total$txt_ans[total$`5097` >= 1 | total$`5098` >= 1 | total$`3006` >= 1 | total$`4756` >= 1]<-1
total$txt_ans = as.factor(total$txt_ans)

total$ctrl_icp <- 0
total$ctrl_icp[total$`337` >= 1 | total$`98597` >= 1 | total$`620` >= 1 | total$`98508` >= 1 | total$`100176` >= 1 | total$`619` >= 1 | total$`11276` >= 1 | total$`141965` >= 1 | total$`98600` >= 1 | total$`3622` >= 1]<-1
total$ctrl_icp = as.factor(total$ctrl_icp)

total$artery<-0
total$artery[total$`107973` >= 1 | total$`110148` >= 1 | total$`108827` >= 1 | total$`109754` >= 1 | total$`107968` >= 1 | total$`107971` >= 1 | total$`107052` >= 1 | total$`109757` >= 1 | total$`107977` >= 1 | total$`109755` >= 1 | total$`110152` >= 1 | total$`174109` >= 1]<-1
total$artery = as.factor(total$artery)

total$cpr<-0
total$cpr[total$`4631` >= 1 | total$`14967` >= 1 | total$`3887` >= 1]<-1
total$cpr = as.factor(total$cpr)


########################### Data clean and recode #############################
# delete gender=="Unknown"
total <- total[total$gender!="Unknown", ]
total$gender = as.factor(total$gender)

# recode race into 7 categories, Caucasian, African American, Native American, Asian/Pacific, Hispanic, Other and Unkown
total$race[total$race=="Biracial" | total$race== "Mid Eastern Indian"] <- "Other"
total$race[total$race=="Asian" | total$race== "Asian/Pacific Islander"| total$race=="Pacific Islander"] <- "Asian/Pacific"
total$race = as.factor(total$race)


# check the frequency of meds and procs, delete ones that only appears in the train or the test set
check <- matrix(NA, ncol(total), 3)
for (i in 9: ncol(total)){
  check[i, ] <- c(colnames(total)[i], sum(total[, i][total$patient_sk %in% demo_train$patient_sk] >= 1), sum(total[, i][total$patient_sk %in% demo_test$patient_sk] >= 1))
}
check <- check[9:ncol(total), ]
summary(as.numeric(check[,2]))  ## check the number of patients in the train that have used each med/procs
summary(as.numeric(check[,3]))  ## check the number of patients in the test that have used each med/procs
select <- check[as.numeric(check[,2]) >= 1 & as.numeric(check[,3]) >= 1, 1]  ## get the med and proc names that applied both in the train and test set
total <- total[, c("patient_sk", "gender", "race", "age_in_years", "death", "los", select)]  ## get the prepared total data, demo+med+proc

# check the frequency of meds and procs, delete ones with very low using rate, we can discuss this part later
check <- matrix(NA, ncol(total), 2)
for (i in 9: ncol(total)){
  check[i, ] <- c(colnames(total)[i], sum(total[, i] >= 1))
}
check <- check[9:ncol(total), ]
summary(as.numeric(check[,2]))  ## more than 75% of the meds or procs only applied to very few patients
# verify the med name and proc id that applies to 30 or more patients
select <- check[as.numeric(check[,2]) >= 30, 1]  ## only 330 meds or procs that applied to 30 or more patients
total <- total[, c("patient_sk", "gender", "race", "age_in_years", "death", "los", select)]  ## get the prepared total data, demo+med+proc
####make death variable numeric####
total$death_code = 0
total$death_code[total$death == "True"] = 1
total$death_code[total$death == "False"] = 0

###make death factor###
total$death = as.factor(total$death)

###make age and los categorical###
total$age_cat = 0
total$age_cat[age_in_years<48]=1
total$age_cat[age_in_years>=48 & age_in_years<58]=2
total$age_cat[age_in_years>=58 & age_in_years<69]=3
total$age_cat[age_in_years>=69 &age_in_years<=90]=4
total$age_cat = as.factor(total$age_cat)

total$los_cat = 0
total$los_cat[total$los<=1]=1
total$los_cat[total$los>1 & total$los<=7]=2
total$los_cat[total$los>7 & total$los<=14]=3
total$los_cat[total$los>14]=4
total$age_Cat = as.factor(total$age_cat)

######################### split into train and test ###########################
Train <- total[total$patient_sk %in% demo_train$patient_sk, ]
Test <- total[total$patient_sk %in% demo_test$patient_sk, ]

########################summary stats and exploratory plots####################
attach(Train)
data = Train
library(gmodels)
table(death)
CrossTable(race, death)
CrossTable(vasopressors,death)
CrossTable(vasodilators,death)
CrossTable(clot_agents,death)
CrossTable(euvolemia,death)
CrossTable(resp_sup,death)
CrossTable(txt_ans,death)
CrossTable(ctrl_icp,death)
CrossTable(artery,death)
CrossTable(cpr,death)
boxplot(los~death)
boxplot(age_in_years~death)


#logisitic regression
set.seed(43)
samp = sample(4910, 4419)
data.train = data[samp,]
data.test = data[-samp,]
glm.fit = glm(death_code ~ age_in_years + race + gender + los + vasopressors + vasodilators + clot_agents + euvolemia + resp_sup + txt_ans + ctrl_icp + artery + cpr + antihypertensives + cardiovascular + antifibrinolytic_agents + analgesics + antibiotics + chemotherapy + antipsychotic + urinary_retention + glaucoma_med + sedatives + antidiabetic + antifungal + antiviral + cholestrolmed + anticonvulsant + deficiencymed + bloodthinners + respiratorymed + diuretic + antiarrhythmic + dermalmed + thrombolytic + antihistamine + antacid + anesthetics + muscle_relaxant + depression_med + steroid + antiemetic + arthritismed + pupildilator + gastrointestinal + dementiamed + parkinsonmed + vaccine + contrast_medium + antiseptic + obstetric + coughmed + bone_marrow_stimulant + bone_health + immunosuppressive + hormone, family = binomial, data = data.train)
glm.probs = predict(glm.fit, newdata = data.test, type='response')
glm.pred = rep("False", 491)
glm.pred[glm.probs>0.5] = "True" 
table(glm.pred, data.test$death)
mean(glm.pred == data.test$death)#86.15%

#variable selection with lasso
library(glmnet)
set.seed(43)
train = sample(4910, 4419)
test = (-train)
X = model.matrix(death_code ~ age_in_years + race + gender + los + vasopressors + vasodilators + clot_agents + euvolemia + resp_sup + txt_ans + ctrl_icp + artery + cpr + antihypertensives + cardiovascular + antifibrinolytic_agents + analgesics + antibiotics + chemotherapy + antipsychotic + urinary_retention + glaucoma_med + sedatives + antidiabetic + antifungal + antiviral + cholestrolmed + anticonvulsant + deficiencymed + bloodthinners + respiratorymed + diuretic + antiarrhythmic + dermalmed + thrombolytic + antihistamine + antacid + anesthetics + muscle_relaxant + depression_med + steroid + antiemetic + arthritismed + pupildilator + gastrointestinal + dementiamed + parkinsonmed + vaccine + contrast_medium + antiseptic + obstetric + coughmed + bone_marrow_stimulant + bone_health + immunosuppressive + hormone, family = binomial, data=data, subset=train)[,-1]
y = data$death_code
y.test = y[test]
grid = 10^seq(10, -2, length = 100)
lasso.fit = glmnet(X[train,], y[train], alpha=1, lambda=grid)
set.seed(10)
cv.out=cv.glmnet(X[train,], y[train], alpha=1)
bestlam = cv.out$lambda.min
predict(lasso.fit, s=bestlam, type='coefficients')
lasso.probs = predict(lasso.fit, s=bestlam, newx=X[test,])
lasso.pred = rep("False", 491)
lasso.pred[lasso.probs>0.5] = "True"
table(lasso.pred, data.test$death)
mean(lasso.pred == data.test$death)#84.92

#new logistic regression model
#new logistic regression model
glm.fit2 = glm(death_code ~ age_in_years + los + vasopressors + vasodilators + euvolemia + resp_sup + txt_ans + ctrl_icp + artery + cpr + antihypertensives +  cardiovascular + analgesics + antipsychotic + antidiabetic + antifungal + cholestrolmed + anticonvulsant + bloodthinners + respiratorymed + diuretic + antiarrhythmic + antihistamine + anesthetics + antiemetic + antiseptic, family = binomial, data = data.train)
glm.probs2 = predict(glm.fit2, newdata = data.test, type='response')
glm.pred2 = rep("False", 491)
glm.pred2[glm.probs2>0.5] = "True"
table(glm.pred2, data.test$death)
mean(glm.pred2 == data.test$death)#85.94%

#LDA
library(MASS)
lda.fit=lda(death_code ~ age_in_years + race + gender + los + vasopressors + vasodilators + clot_agents + euvolemia + resp_sup + txt_ans + ctrl_icp + artery + cpr + antihypertensives + cardiovascular + antifibrinolytic_agents + analgesics + antibiotics + chemotherapy + antipsychotic + urinary_retention + glaucoma_med + sedatives + antidiabetic + antifungal + antiviral + cholestrolmed + anticonvulsant + deficiencymed + bloodthinners + respiratorymed + diuretic + antiarrhythmic + dermalmed + thrombolytic + antihistamine + antacid + anesthetics + muscle_relaxant + depression_med + steroid + antiemetic + arthritismed + pupildilator + gastrointestinal + dementiamed + parkinsonmed + vaccine + contrast_medium + antiseptic + obstetric + coughmed + bone_marrow_stimulant + bone_health + immunosuppressive + hormone, data = data.train)
lda.fit
lda.pred=predict(lda.fit, data.test)
table(lda.pred$class, data.test$death_code)
mean(lda.pred$class==data.test$death_code)#85.74%

#QDA
library(MASS)
qda.fit=qda(death_code ~ age_in_years + race + gender + los + vasopressors + vasodilators + clot_agents + euvolemia + resp_sup + txt_ans + ctrl_icp + artery + cpr + antihypertensives + cardiovascular + antifibrinolytic_agents + analgesics + antibiotics + chemotherapy + antipsychotic + urinary_retention + glaucoma_med + sedatives + antidiabetic + antifungal + antiviral + cholestrolmed + anticonvulsant + deficiencymed + bloodthinners + respiratorymed + diuretic + antiarrhythmic + dermalmed + thrombolytic + antihistamine + antacid + anesthetics + muscle_relaxant + depression_med + steroid + antiemetic + arthritismed + pupildilator + gastrointestinal + dementiamed + parkinsonmed + vaccine + contrast_medium + antiseptic + obstetric + coughmed + bone_marrow_stimulant + immunosuppressive + hormone, data = data.train)
qda.fit
qda.pred=predict(qda.fit, data.test)
table(qda.pred$class, data.test$death_code)
mean(qda.pred$class==data.test$death_code)#78.41

#Random Forest
library(randomForest)
rf.model = randomForest(death_code ~ age_in_years + race + gender + los + vasopressors + vasodilators + clot_agents + euvolemia + resp_sup + txt_ans + ctrl_icp + artery + cpr + antihypertensives + antifibrinolytic_agents + analgesics + antibiotics + antipsychotic + urinary_retention + glaucoma_med + sedatives + antidiabetic + antifungal + antiviral + cholestrolmed + anticonvulsant + deficiencymed + bloodthinners + asthmamed data = data.train, mtry=4, ntree=1000)
rf.preds = predict(rf.model, data.test, type='response')
table(rf.preds, data.test$death)
mean(rf.preds == data.test$death)
importance(rf.model)

#support vector machine
library(e1071)
#linear kernel
tune.out=tune(svm, death ~ age_in_years + race + gender + los + vasopressors + vasodilators + clot_agents + euvolemia + resp_sup + txt_ans + ctrl_icp + artery + cpr, data=data.train, kernel='linear', ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100)))
summary(tune.out)
svm.fit=tune.out$best.model
svm.preds=predict(svm.fit, data.test)
table(svm.preds,data.test$death)
mean(svm.preds == data.test$death)
#radial kernel
tune.out2=tune(svm, death ~ age_in_years + race + gender + los + vasopressors + vasodilators + clot_agents + euvolemia + resp_sup + txt_ans + ctrl_icp + artery + cpr, data=data.train, kernel='radial', ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100)))
summary(tune.out2)
svm.fit2=tune.out2$best.model
svm.preds2=predict(svm.fit2, data.test)
table(svm.preds2,data.test$death)
mean(svm.preds2 == data.test$death)


#####################best subset for logistic##################
library(leaps)
regfit.full=regsubsets(death_code ~ age_in_years + los + vasopressors + vasodilators + euvolemia + resp_sup + txt_ans + ctrl_icp + artery + cpr + antifibrinolytic_agents + race, data.train)
summary(regfit.full)
