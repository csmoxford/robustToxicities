############################################################################################
# Toxicity utility and matching programs
############################################################################################
ls.cat=c("Blood and lymphatic system disorders",
         "Cardiac disorders",
         "Congenital, familial and genetic disorders",
         "Ear and labyrinth disorders",
         "Endocrine disorders",
         "Eye disorders",
         "Gastrointestinal disorders",
         "General disorders and administration site conditions",
         "Hepatobiliary disorders",
         "Immune system disorders",
         "Infections and infestations",
         "Injury, poisoning and procedural complications",
         "Investigations",
         "Metabolism and nutrition disorders",
         "Musculoskeletal and connective tissue disorders",
         "Neoplasms benign, malignant and unspecified (incl cysts and polyps)",
         "Nervous system disorders",
         "Pregnancy, puerperium and perinatal conditions",
         "Psychiatric disorders",
         "Renal and urinary disorder",
         "Reproductive system and breast disorders",
         "Respiratory, thoracic and mediastinal disorders",
         "Skin and subcutaneous tissue disorders",
         "Social circumstances",
         "Surgical and medical procedures",
         "Vascular disorders",
         "Other")


############################################################################################
# Remove leading and trailing spaces, punctuation, numbers and make strings lowercase for easier matching
clean.string=function(string){
  if(!is.na(string)){
  string1=gsub("[[:punct:]]","",string)
  if(string1!=string){flag.pun=1}else{flag.pun=0}
  string2=gsub("[[0-9]]","",string1)
  if(string2!=string){flag.num=1}else{flag.num=0}
  string2=tolower(string2)
  string2=gsub("^ *|(?<= ) | *$", "", string2, perl=T)
  return(list(string=string2,flag.pun=flag.pun,flam.num=flag.num))
  }
  return(list(string="",flag.pun=0,flam.num=0))
}

############################################################################################
# Find the right category
category.link=function(category,ls.catagory=ls.cat){
  return(which(ls.catagory==category)[1])
}


############################################################################################
renumber_database=function(database){
  database=database[order(database$CatID,database$toxicity_disp),]
  a=dim(database)[1]
  database$inCatID[1]=1
  for(i in 2:a){
    if(database$CatID[i]==database$CatID[i-1]){
      database$inCatID[i]=database$inCatID[i-1]+1
    } else {
      database$inCatID[i]=1
    }
  }
  database$toxID=database$CatID*1000+database$inCatID
  return(database)
}


number.toxicities=function(database){
  database=database[order(database$ass_category,database$ass_toxicity_disp),]

  database$ass_toxID=0
  database$ass_toxID[1]=1
  j=1
  for(i in 2:length( database$ass_toxID)){
    if(database$ass_toxicity_disp[i] != database$ass_toxicity_disp[i-1]){
      j=j+1
    }
    database$ass_toxID[i]=j
  }
  return(database)
}
