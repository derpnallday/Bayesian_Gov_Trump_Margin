# Using Stan-Ydich-XnomSsubj-MbernBetaOmegaKappa.R 
#------------------------------------------------------------------------------- 

# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!

#------------------------------------------------------------------------------- 

# Load The data 
house = read.csv("House.csv")
senate = read.csv("Senate.csv")

#factor party affiliation var
house$Party = factor(house$Party)
senate$Party = factor(senate$Party)


#clean data for only important vars
keep = c("Party","Approval")

house = house[keep]
senate = senate[keep]

yName = "Approval" # column name for 0,1 values
sName = "Party" # column name for subject ID


fileNameRootHouse = "House-Jags-Ydich-XnomSsubj-MbernBetaOmegaKappa-"
fileNameRootSenate= "Senate-Jags-Ydich-XnomSsubj-MbernBetaOmegaKappa-" 
graphFileType = "eps" 


#------------------------------------------------------------------------------- 

#explore the data
par(mfrow=c(1,2))

#see how how house votes with/against trump based on party
overlayH = table(house$Party,house$Approval)

h = barplot(overlayH, main=" House Trump voting frequency by party",
        xlab="For/Against Trump Policies", col=c("darkblue","red"),
        legend = rownames(overlayH), beside=TRUE,
        names.arg=c("against", "with"),
        ylim=c(0,350))

#get frequency for amount of members
hx0 <- overlayH[,1]
hx1 = overlayH[,2]
hFreq = append(hx0,hx1)

#label graph with freq
text(c(1.5,2.5,4.5,5.5),hFreq+15,labels=as.character(hFreq))

#label graph with total house members
hTotal = toString(sum(hFreq))
hStr = paste("Total=", hTotal)
text(2, 300,labels=hStr)


#see how how senate votes with/against trump based on party
overlayS = table(senate$Party,senate$Approval)

barplot(overlayS, main="Senate Trump voting frequency by party",
        xlab="For/Against Trump Policies", col=c("darkblue","red"),
        legend = rownames(overlayS), beside=TRUE,
        names.arg=c("against", "with"),
        ylim=c(0,100))

#get frequency for amount of members
sx0 <- overlayS[,1]
sx1 = overlayS[,2]
sFreq = append(sx0,sx1)

#label graph with freq
text(c(1.5,2.5,4.5,5.5),sFreq+6,labels=as.character(sFreq))

#label graph with total senate members
sTotal = toString(sum(sFreq))
sStr = paste("Total=", sTotal)
text(2, 80,labels=sStr)

#------------------------------------------------------------------------------- 
# HOUSE
#------------------------------------------------------------------------------- 

# Work with priors model
source("house_model.R")

#------------------------------------------------------------------------------- 

# Generate MCMC chains on Data

houseCoda = genMCMC( data=house , sName=sName , yName=yName , 
                    numSavedSteps=20000 , saveName=fileNameRootHouse , thinSteps=10 )



#------------------------------------------------------------------------------- 

# Display diagnostics of chains, for each parameter

parameterNames = varnames(houseCoda) # get all parameter names for reference
for ( parName in parameterNames[c(1:3,length(parameterNames))] ) { 
  diagMCMC( codaObject=houseCoda , parName=parName , 
            saveName=fileNameRootHouse , saveType=graphFileType )
}

#------------------------------------------------------------------------------- 

# Get summary statistics of chain:
summaryInfo = smryMCMC( houseCoda , compVal=0.5 , 
                        diffIdVec=c(1,2), 
                        compValDiff=-1 ,
                        saveName=fileNameRootHouse )

# Display posterior information:
plotMCMC( houseCoda , data=house , sName=sName , yName=yName , 
          compVal=c(0.49) , rope=c(0.4,0.49) , 
          diffIdVec=c(1,2),              # Therapeutic touch
          compValDiff=-1, ropeDiff = c(-1.05,-0.95) ,
          saveName=fileNameRootHouse , saveType=graphFileType )


#------------------------------------------------------------------------------- 
# SENATE
#------------------------------------------------------------------------------- 


# Work with senate priors
source("senate_model.R")


#------------------------------------------------------------------------------- 


senateCoda = genMCMC( data=senate , sName=sName , yName=yName , 
                      numSavedSteps=20000 , saveName=fileNameRootSenate , thinSteps=10 )

#------------------------------------------------------------------------------- 

# Display diagnostics of chains, for each parameter

parameterNames = varnames(senateCoda) # get all parameter names for reference
for ( parName in parameterNames[c(1:3,length(parameterNames))] ) { 
  diagMCMC( codaObject=senateCoda , parName=parName , 
            saveName=fileNameRootSenate , saveType=graphFileType )
}

#------------------------------------------------------------------------------- 


# Get summary statistics of chain:
summaryInfo = smryMCMC( senateCoda , compVal=0.5 , 
                        diffIdVec=c(1,2),  # Therapeutic touch
                        # diffIdVec=c(38,60,2),  # ESP Tressoldi et al.
                        compValDiff=-1 ,
                        saveName=fileNameRootSenate )


# Display posterior information:
plotMCMC( senateCoda , data=senate , sName=sName , yName=yName , 
          compVal=0.49 , rope=c(0.4,0.49) ,
          diffIdVec=c(1,2),           
          compValDiff=-1, ropeDiff = c(-1.1,-0.9) ,
          saveName=fileNameRootSenate , saveType=graphFileType )

#------------------------------------------------------------------------------- 