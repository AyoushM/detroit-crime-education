# Reading datasets
alldata = read.csv("Aggregate Data.csv")
schoolnames = read.csv("School names.csv")
zipdata = read.csv("Wayne_Schools_ZIPS.csv")

# Extracting School Names
# 1st Pass
schoolnames$cleaned = sub("0", "", schoolnames$x)
schoolnames$cleaned = sub("1", "", schoolnames$cleaned)
schoolnames$cleaned = sub("2", "", schoolnames$cleaned)
schoolnames$cleaned = sub("3", "", schoolnames$cleaned)
schoolnames$cleaned = sub("4", "", schoolnames$cleaned)
schoolnames$cleaned = sub("5", "", schoolnames$cleaned)
schoolnames$cleaned = sub("6", "", schoolnames$cleaned)
schoolnames$cleaned = sub("7", "", schoolnames$cleaned)
schoolnames$cleaned = sub("8", "", schoolnames$cleaned)
schoolnames$cleaned = sub("9", "", schoolnames$cleaned)
schoolnames$cleaned = sub("#", "", schoolnames$cleaned)

# 2nd Pass
schoolnames$cleaned = sub("0", "", schoolnames$cleaned)
schoolnames$cleaned = sub("1", "", schoolnames$cleaned)
schoolnames$cleaned = sub("2", "", schoolnames$cleaned)
schoolnames$cleaned = sub("3", "", schoolnames$cleaned)
schoolnames$cleaned = sub("4", "", schoolnames$cleaned)
schoolnames$cleaned = sub("5", "", schoolnames$cleaned)
schoolnames$cleaned = sub("6", "", schoolnames$cleaned)
schoolnames$cleaned = sub("7", "", schoolnames$cleaned)
schoolnames$cleaned = sub("8", "", schoolnames$cleaned)
schoolnames$cleaned = sub("9", "", schoolnames$cleaned)

#Final/Selective Pass
schoolnames$cleaned = sub("1", "", schoolnames$cleaned)
schoolnames$cleaned = sub(" \\()", "", schoolnames$cleaned)

# Getting school ZIP data
school.location = subset(zipdata, select = c(BuildingName, ZIP_CODE, Latitude, Longitude))
school.location$cleaned = school.location$BuildingName

# Merging
school.data = merge(schoolnames, school.location, by = "cleaned", all.x = TRUE)

# 20 rows of missing data were filled in manually using maps and MS Excel

# Restructuring the data
backup = school.data
backup = backup[, -c(2,4)]
backup$Location.Name = backup$x
colnames(backup)[colnames(backup) == "cleaned"] = "School.Name"
colnames(backup)[colnames(backup) == "ZIP_CODE"] = "ZIP.Code"
backup = backup[, -2]
school.data = backup

# Removing duplicate rows
backup = school.data
backup = unique(backup)
backup = backup[!(backup$School.Name == "Henry Ford Academy" & backup$ZIP.Code == 48203),]

# Storing
school.data = backup

# Integrating into Educational Records
EduData.w.Loc = merge(alldata, school.data, by = "Location.Name", all.x = TRUE)
EduData.w.Loc[order(EduData.w.Loc$Location.Name, EduData.w.Loc$enrollyear),]

# Exporting to CSV
write.csv(EduData.w.Loc, "Education_Data_wLoc.csv")
