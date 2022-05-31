import pandas as pd
import numpy as np
import time

class SplitData:

    count = 0
    start = 0
    lastTime = 0

    def __init__(self, startYear, endYear, readCurrentAnalyticsDfs=False):
        self.startingYear = startYear
        self.endingYear = endYear # one greater than latest year of data
        self.dfs = dict()
        if readCurrentAnalyticsDfs:
            for year in range(startYear, endYear):
                self.dfs[year] = pd.read_csv("faceoff_analytics_" + str(year) + ".csv")

    def loadMergedData(self):
        for year in range(self.startingYear, self.endingYear):
            print("loading data for", year)
            self.dfs[year] = pd.read_csv('merged_pbp_with_shots_' + str(year) + '-' + str(year+1) + ".csv")

    def splitData(self):
        for year in range(self.startingYear, self.endingYear):
            cumulativeCounter = 1
            recordsPerFile = 50000
            initialIndex = 0
            cutoffIndex = recordsPerFile
            df = pd.read_csv("EH_pbp_query_" + str(year) + str(year + 1) + ".csv")
            lastIndex = len(df[df.columns[0]])
            while initialIndex < lastIndex:
                df_1 = df.iloc[initialIndex:(cutoffIndex+1)]
                print("EH_pbp_" + str(year) + "-" + str(year+1) + "_Part_" + str(cumulativeCounter))
                df_1.to_csv("EH_pbp_" + str(year) + "-" + str(year+1) + "_Part_" + str(cumulativeCounter) + ".csv")
                initialIndex = cutoffIndex + 1
                cutoffIndex = initialIndex + recordsPerFile - 1
                cumulativeCounter += 1

    def readSplitData(self):
        self.dfs = dict()
        for year in range(self.startingYear, self.endingYear):
            print("importing for", str(year))
            self.dfs[year] = pd.read_csv("EH_pbp_" + str(year) + "-" + str(year+1) + "_Part_1.csv")
            for part in range(1, 100):
                print(str(year), str(part), "in progress")
                try:
                    self.dfs[year] = pd.concat([self.dfs[year], pd.read_csv("EH_pbp_" + str(year) + "-" + str(year+1) + "_Part_" + str(part) + ".csv")])
                except:
                    break

    def xG_from_faceoffs(self):
        self.computedColumns()

    def findLastFaceoffTime(self, currentYear, row):
        # Precondition: each df in self.dfs is sorted in oldest-to-newst order of event occurring
        df = self.dfs[currentYear]
        index = row['index']  # slightly shady.. fix later! .. this is a substitute for getting row's index directly
        movingIndex = index
        while (movingIndex > 0 and movingIndex < len(self.dfs[currentYear].index)):
            if self.dfs[currentYear].at[movingIndex, 'event_type'] == 'FAC':
               return self.dfs[currentYear].at[movingIndex, 'game_seconds']
            movingIndex -= 1
        return np.NaN

    def findZoneChangesSinceLastFaceoff(self, currentYear, row):
        index = row['index']  # slightly shady.. fix later! .. this is a substitute for getting row's index directly
        if index < 0 or index >= len(self.dfs[currentYear].index):
            return np.NaN
        movingIndex = index
        zoneChangeCount = 0
        while (movingIndex >= 0 and movingIndex < len(self.dfs[currentYear].index) and self.dfs[currentYear].at[movingIndex, 'time_since_faceoff'] > 0):
            zoneChanges = self.dfs[currentYear].at[movingIndex, 'zone_changes_since_last_event']
            if (zoneChanges > 0):
                zoneChangeCount += zoneChanges
            movingIndex -= 1
        return zoneChangeCount

    def expectedGoalsSinceFaceoff(self, currentYear, row):
        index = row['index']  # slightly shady.. fix later! .. this is a substitute for getting row's index directly
        if index < 0 or index >= len(self.dfs[currentYear].index):
            return np.NaN
        if (self.dfs[currentYear].at[index, 'event_type'] == 'FAC'):
            return 0
        if (self.dfs[currentYear].at[index, 'event_zone'] == 'Neu'):
            return 0
        movingIndex = index
        cumulativeExpectedGoals = 0
        while (movingIndex >= 0 and movingIndex < len(self.dfs[currentYear].index) and self.dfs[currentYear].at[movingIndex, 'time_since_faceoff'] > 0):
            currentCumulativeXGoals = self.dfs[currentYear].at[movingIndex, 'xG_since_faceoff']
            if self.dfs[currentYear].at[movingIndex, 'zone_changes_since_last_event'] == 0:
                xGoals = self.dfs[currentYear].at[movingIndex, 'xGoal']
                if (self.dfs[currentYear].at[movingIndex,'event_type'] == 'GOAL' or self.dfs[currentYear].at[movingIndex,'event_type'] == 'MISS' or self.dfs[currentYear].at[movingIndex,'event_type'] == 'SHOT'):
                    if(xGoals > 0):
                        cumulativeExpectedGoals += xGoals
                movingIndex -= 1
            else:
                cumulativeExpectedGoals -= currentCumulativeXGoals
                # Take out any expected goals reflected in current totals that were accumulated outside zone (i.e. before entry to current zone)
                # At this point, currentCumulativeXGoals is cumulative expected goals at most recent past event outside current zone
                break
        return cumulativeExpectedGoals

    def faceoffWinningTeamExpectedGoalsSinceFaceoff(self, currentYear, row):
        index = row['index']  # slightly shady.. fix later! .. this is a substitute for getting row's index directly
        if index < 0 or index >= len(self.dfs[currentYear].index):
            return np.NaN
        if (self.dfs[currentYear].at[index, 'event_type'] == 'FAC'):
            return 0
        if (self.dfs[currentYear].at[index, 'event_zone'] == 'Neu'):
            return 0
        movingIndex = index
        cumulativeExpectedGoals = 0
        while (movingIndex >= 0 and movingIndex < len(self.dfs[currentYear].index) and self.dfs[currentYear].at[movingIndex, 'time_since_faceoff'] > 0):
            currentCumulativeXGoals = self.dfs[currentYear].at[movingIndex, 'faceoff_winning_team_xG_since_faceoff']
            xGoals = self.dfs[currentYear].at[movingIndex, 'xGoal']
            if (self.dfs[currentYear].at[movingIndex,'event_type'] == 'GOAL' or self.dfs[currentYear].at[movingIndex,'event_type'] == 'MISS' or self.dfs[currentYear].at[movingIndex,'event_type'] == 'SHOT'):
                if self.dfs[currentYear].at[movingIndex,'last_faceoff_winner_on_offense'] == True:
                    if(xGoals > 0):
                        cumulativeExpectedGoals += xGoals
            movingIndex -= 1
        return cumulativeExpectedGoals

    def faceoffLosingTeamExpectedGoalsSinceFaceoff(self, currentYear, row):
        index = row['index']  # slightly shady.. fix later! .. this is a substitute for getting row's index directly
        if index < 0 or index >= len(self.dfs[currentYear].index):
            return 0
        if (self.dfs[currentYear].at[index, 'event_type'] == 'FAC'):
            return 0
        if (self.dfs[currentYear].at[index, 'event_zone'] == 'Neu'):
            return 0
        movingIndex = index
        cumulativeExpectedGoals = 0
        while (movingIndex >= 0 and movingIndex < len(self.dfs[currentYear].index) and self.dfs[currentYear].at[movingIndex, 'time_since_faceoff'] > 0):
            currentCumulativeXGoals = self.dfs[currentYear].at[movingIndex, 'faceoff_losing_team_xG_since_faceoff']
            xGoals = self.dfs[currentYear].at[movingIndex, 'xGoal']
            if (self.dfs[currentYear].at[movingIndex,'event_type'] == 'GOAL' or self.dfs[currentYear].at[movingIndex,'event_type'] == 'MISS' or self.dfs[currentYear].at[movingIndex,'event_type'] == 'SHOT'):
                if self.dfs[currentYear].at[movingIndex,'last_faceoff_winner_on_offense'] == False:
                    if(xGoals > 0):
                        cumulativeExpectedGoals += xGoals
            movingIndex -= 1
        return cumulativeExpectedGoals

    def applyIndex(self, row):
        returnValue = SplitData.count
        SplitData.count += 1
        return returnValue

    def time_adjust(self, time1, time2):
        return time1-time2
    

    # def identifyZone(self, row):
    #     if ("Off. Zone" in str(row['event_description'])):
    #         return "OFFENSIVE"
    #     elif ("Def. Zone" in str(row['event_description'])):
    #         return "DEFENSIVE"
    #     elif ("Neu. Zone" in str(row['event_description'])):
    #         return "NEUTRAL"
    #     else:
    #         return ""

    # Precondition: events sorted in chronological order
    def zoneChangesSinceLastEvent(self, key, row):
        index = row['index'] # slightly shady.. fix later! .. this is a substitute for getting row's index directly
        if (index == 0 or index >= len(self.dfs[key].index)):
            return 0
        # if row['event_type'] == "CHANGE":
        #     return 0
        priorIndex = index - 1
        priorEventZone = (self.dfs[key]).at[priorIndex, 'event_zone']
        priorEventTeam = (self.dfs[key]).at[priorIndex, 'event_team']
        while (not (priorEventZone == "Off" or priorEventZone == "Neu" or priorEventZone == "Def")):
            priorIndex -= 1
            if priorIndex < 0:
                return 0
            priorEventZone = (self.dfs[key]).at[priorIndex, 'event_zone']
            priorEventTeam = (self.dfs[key]).at[priorIndex, 'event_team']
        currentEventZone = (self.dfs[key]).at[index, 'event_zone']
        # If an event doesn't have a zone attached to it, it should not be counted as a zone-changing event.
        if not (currentEventZone == "Off" or currentEventZone == "Def" or currentEventZone == "Neu"):
            return 0
        if not (priorEventZone == "Off" or priorEventZone == "Def" or priorEventZone == "Neu"):
            return 0
        currentEventTeam = (self.dfs[key]).at[index, 'event_team']
        if (priorEventZone == currentEventZone):
            if priorEventTeam == currentEventTeam:
                return 0
            else:
                if currentEventZone == "Neu":
                    return 0
                else:
                    return 2
        else:
            if priorEventTeam != currentEventTeam and (not priorEventZone == "Neu") and (not currentEventZone == "Neu"):
                return 0
            if priorEventTeam == currentEventTeam and priorEventZone != currentEventZone and (not priorEventZone == "Neu") and (not currentEventZone == "Neu"): # NEW
                return 2
            else:
                return 1

    def isStartOfOffensiveZoneTime(self, currentYear, row):
        if (row['index'] < 0 or row['index'] >= len(self.dfs[currentYear].index)):
            return np.NaN
        if (self.dfs[currentYear].at[row['index'], 'event_zone'] == "Neu"):
            return False
        if (self.dfs[currentYear].at[row['index'], 'zone_changes_since_last_event'] > 0): # Given: current event is either offensive or defensive
            return True;
        # if (self.dfs[currentYear]).at[row['index'], 'event_zone'] != "Off": # Current event must be in offensive zone to potentially have start of offensive zone time
        #     return False
        # if (self.dfs[currentYear]).at[row['index'], 'zone_changes_since_last_event'] > 0: # Must be zone change if we are starting offensive zone time
        #     return True
        return False

    def isStartOfNeutralZoneTime(self, currentYear, row):
        if (row['index'] < 0 or row['index'] >= len(self.dfs[currentYear].index)):
            return np.NaN
        if (self.dfs[currentYear]).at[row['index'], 'event_zone'] != "Neu": # Current event must be in offensive zone to potentially have start of offensive zone time
            return False
        if (self.dfs[currentYear]).at[row['index'], 'zone_changes_since_last_event'] > 0: # Must be zone change if we are starting offensive zone time
            return True
        #if self.time_adjust(self.dfs[currentYear].at[row['index'],'clock_time'], str(0)) == 1200:
        #   return True
        return False

    def isStartOfDefensiveZoneTime(self, currentYear, row):
        if (row['index'] < 0 or row['index'] >= len(self.dfs[currentYear].index)):
            return np.NaN
        if (self.dfs[currentYear]).at[row['index'], 'event_zone'] == "Neu": # Current event must be in defensive zone to potentially have start of defensive zone time
            return False
        if (self.dfs[currentYear]).at[row['index'], 'zone_changes_since_last_event'] > 0: # Must be zone change if we are starting defensive zone time
            return True
        return False


    def isEndOfOffensiveZoneTime(self, currentYear, row):
        if (row['index'] < 0 or row['index'] >= len(self.dfs[currentYear].index)):
            return np.NaN
        cond1 = (self.dfs[currentYear]).at[row['index'], 'event_zone'] != "Off" and (self.dfs[currentYear]).at[row['index'], 'zone_changes_since_last_event'] > 0 # Current event not in offensive zone and there was a zone change since last event
        cond2 = (self.dfs[currentYear]).at[row['index'], 'event_zone'] == "Off" and (self.dfs[currentYear]).at[row['index'], 'zone_changes_since_last_event'] == 2 # Current event in offensive zone but we crossed two zones since last event
        if cond1 or cond2:
            index = row['index']
            currentEventTeam = (self.dfs[currentYear]).at[index, 'event_team']
            currentEventZone = (self.dfs[currentYear]).at[index, 'event_zone']
            priorIndex = index - 1
            priorEventZone = (self.dfs[currentYear]).at[priorIndex, 'event_zone']
            priorEventTeam = (self.dfs[currentYear]).at[priorIndex, 'event_team']
            while (not (priorEventZone == "Off" or priorEventZone == "Neu" or priorEventZone == "Def")):
                priorIndex -= 1
                if priorIndex < 0 or len(self.dfs[currentYear].index):
                    return 0
                priorEventZone = (self.dfs[currentYear]).at[priorIndex, 'event_zone']
                priorEventTeam = (self.dfs[currentYear]).at[priorIndex, 'event_team']
            if priorEventZone == "Off":
                return True
            if priorEventZone == "Def" and priorEventTeam != currentEventTeam:
                return True
            if priorEventZone != currentEventZone and priorEventTeam == currentEventTeam and (not priorEventZone == "Neu"): # NEW
                return True
        return False

    def isEndOfNeutralZoneTime(self, currentYear, row):
        if (row['index'] < 0 or row['index'] >= len(self.dfs[currentYear].index)):
            return np.NaN
        cond = (self.dfs[currentYear]).at[row['index'], 'event_zone'] != "Neu" and (self.dfs[currentYear]).at[row['index'], 'zone_changes_since_last_event'] > 0
        if cond:
            index = row['index']
            currentEventTeam = (self.dfs[currentYear]).at[index, 'event_team']
            currentEventZone = (self.dfs[currentYear]).at[index, 'event_zone']
            priorIndex = index - 1
            priorEventZone = (self.dfs[currentYear]).at[priorIndex, 'event_zone']
            priorEventTeam = (self.dfs[currentYear]).at[priorIndex, 'event_team']
            while (not (priorEventZone == "Off" or priorEventZone == "Neu" or priorEventZone == "Def")):
                priorIndex -= 1
                if priorIndex < 0 or index >= len(self.dfs[currentYear].index):
                    return 0
                priorEventZone = (self.dfs[currentYear]).at[priorIndex, 'event_zone']
                priorEventTeam = (self.dfs[currentYear]).at[priorIndex, 'event_team']
            if priorEventZone == "Neu":
                return True
        return False

    def isEndOfDefensiveZoneTime(self, currentYear, row):
        if (row['index'] < 0 or row['index'] >= len(self.dfs[currentYear].index)):
            return np.NaN
        cond1 = (self.dfs[currentYear]).at[row['index'], 'event_zone'] != "Off" and (self.dfs[currentYear]).at[row['index'], 'zone_changes_since_last_event'] > 0 # Current event not in offensive zone and there was a zone change since last event
        cond2 = (self.dfs[currentYear]).at[row['index'], 'event_zone'] == "Off" and (self.dfs[currentYear]).at[row['index'], 'zone_changes_since_last_event'] == 2 # Current event in offensive zone but we crossed two zones since last event
        if cond1 or cond2:
            index = row['index']
            currentEventTeam = (self.dfs[currentYear]).at[index, 'event_team']
            currentEventZone = (self.dfs[currentYear]).at[index, 'event_zone']
            priorIndex = index - 1
            priorEventZone = (self.dfs[currentYear]).at[priorIndex, 'event_zone']
            priorEventTeam = (self.dfs[currentYear]).at[priorIndex, 'event_team']
            while (not (priorEventZone == "Off" or priorEventZone == "Neu" or priorEventZone == "Def")):
                priorIndex -= 1
                if priorIndex < 0:
                    return 0
                priorEventZone = (self.dfs[currentYear]).at[priorIndex, 'event_zone']
                priorEventTeam = (self.dfs[currentYear]).at[priorIndex, 'event_team']
            if priorEventZone == "Off":
                return True
            if priorEventZone == "Def" and priorEventTeam != currentEventTeam:
                return True
            if priorEventZone != currentEventZone and priorEventTeam == currentEventTeam and (not priorEventZone == "Neu"): # NEW
                return True
        return False

    def computeNeutralZoneTime(self, currentYear, row):
        index = row['index']  # slightly shady.. fix later! .. this is a substitute for getting row's index directly
        if (row['index'] < 0 or row['index'] >= len(self.dfs[currentYear].index)):
            return np.NaN
        movingIndex = index
        cumulativeNeutralZoneTime = 0      
        if (self.dfs[currentYear].at[index, 'end_neutral_zone_time'] == True):
            while (movingIndex >= 1 and self.dfs[currentYear].at[movingIndex, 'begin_neutral_zone_time'] != True):
                movingIndex -= 1
            cumulativeNeutralZoneTime = abs(self.dfs[currentYear].at[movingIndex,'game_seconds'] - row['game_seconds'])
        return cumulativeNeutralZoneTime
        

    def computeDefensiveZoneTime(self, currentYear, row):
        index = row['index']  # slightly shady.. fix later! .. this is a substitute for getting row's index directly
        if (row['index'] < 0 or row['index'] >= len(self.dfs[currentYear].index)):
            return np.NaN
        movingIndex = index-1
        cumulativeOffensiveZoneTime = 0
        if (self.dfs[currentYear].at[index, 'end_offensive_zone_time'] == True):
            while (movingIndex >= 0 and self.dfs[currentYear].at[movingIndex, 'begin_offensive_zone_time'] != True):
                movingIndex -= 1
                movingIndex -= 1
            cumulativeOffensiveZoneTime = abs(row['game_seconds'] - self.dfs[currentYear].at[movingIndex,'game_seconds'])
        return cumulativeOffensiveZoneTime

    def computeOffensiveZoneTime(self, currentYear, row):
        index = row['index']  # slightly shady.. fix later! .. this is a substitute for getting row's index directly
        if (row['index'] < 0 or row['index'] >= len(self.dfs[currentYear].index)):
            return np.NaN
        movingIndex = index-1
        cumulativeOffensiveZoneTime = 0
        if (self.dfs[currentYear].at[index, 'end_offensive_zone_time'] == True):
            while (movingIndex >= 0 and self.dfs[currentYear].at[movingIndex, 'begin_offensive_zone_time'] != True):
                movingIndex -= 1
            cumulativeOffensiveZoneTime = abs(row['game_seconds'] - self.dfs[currentYear].at[movingIndex,'game_seconds'])
        return cumulativeOffensiveZoneTime
    
    def computeOffensiveZoneFACTime(self, currentYear, row):
        index = row['index']  # slightly shady.. fix later! .. this is a substitute for getting row's index directly
        if (row['index'] < 0 or row['index'] >= len(self.dfs[currentYear].index)):
            return np.NaN
        movingIndex = index+1
        cumulativeOffensiveZoneTime = 0
        if (self.dfs[currentYear].at[index, 'event_type'] == 'FAC' and self.dfs[currentYear].at[index, 'event_zone'] != 'Neu'):
            while (movingIndex < len(self.dfs[currentYear].index) - 1 and self.dfs[currentYear].at[movingIndex, 'end_offensive_zone_time'] != True and self.dfs[currentYear].at[movingIndex, 'event_type'] != 'FAC'):
                movingIndex += 1
            cumulativeOffensiveZoneTime = abs(row['game_seconds'] - self.dfs[currentYear].at[movingIndex,'game_seconds'])
        return cumulativeOffensiveZoneTime
    
    def getToOffensiveZone(self, currentYear, row):
        index = row['index']
        if (row['index'] < 0 or row['index'] >= len(self.dfs[currentYear].index)):
            return np.NaN
        movingIndex = index+1
        if (self.dfs[currentYear].at[index,'event_zone'] == 'Neu' and self.dfs[currentYear].at[index, 'event_type'] == 'FAC'):
            while(movingIndex < len(self.dfs[currentYear].index) - 1 and self.dfs[currentYear].at[movingIndex, 'event_zone'] != 'Off' and self.dfs[currentYear].at[movingIndex, 'event_zone'] != 'Def'):
                movingIndex += 1
            if self.dfs[currentYear].at[movingIndex, 'event_zone'] == 'Off':
                if self.dfs[currentYear].at[movingIndex, 'event_team'] == self.dfs[currentYear].at[index, 'event_team']:
                    return True
                else:
                    return False
            if self.dfs[currentYear].at[movingIndex, 'event_zone'] == 'Def':
                if self.dfs[currentYear].at[movingIndex, 'event_team'] == self.dfs[currentYear].at[index, 'event_team']:
                    return False
                else:
                    return True
                
    def offensiveZoneNEUTime(self, currentYear,row):
        index = row['index']
        if (row['index'] < 0 or row['index'] >= len(self.dfs[currentYear].index)):
            return np.NaN
        movingIndex = index+1
        timeInZone = 0
        if(self.dfs[currentYear].at[index, 'getToOffensiveZone'] == True):
            while(movingIndex < len(self.dfs[currentYear].index) - 1 and self.dfs[currentYear].at[movingIndex, 'end_offensive_zone_time'] != True):
                movingIndex += 1
            timeInZone =  self.dfs[currentYear].at[movingIndex, 'offensive_zone_time']
        return timeInZone
                

    def trunc_time_to_intervals(self, untruncatedTime, intervalSize):
        categorization = untruncatedTime // intervalSize # Note the integer division
        return categorization * intervalSize

    def auditTiming(self):
        currentTime = time.time()
        #print("time:", str(int(currentTime - SplitData.lastTime)), "seconds")
        SplitData.lastTime = currentTime

    def computedColumns(self):
        print(self.dfs.keys())
        for key in self.dfs.keys():
            SplitData.count = 0
            initialKey = key
            assert (key == initialKey)
            (self.dfs[key]) = (self.dfs[key]).sort_values(by=['game_id_x', 'game_seconds', 'event_index'])
            assert (key == initialKey)
            print("applying indices")
            (self.dfs[key])['index'] = (self.dfs[key]).apply(lambda row: self.applyIndex(row),
                                                             axis=1)
            #(self.dfs[key]) = (self.dfs[key]).head(1000)  # Use for testing purposes only
            assert(key == initialKey)
            print("computing last faceoff time")
            (self.dfs[key])['last_faceoff_time'] = (self.dfs[key]).apply(lambda row: self.findLastFaceoffTime(key, row),
                                                                         axis=1)
            #print("last faceoff time determination complete")
            #self.auditTiming()
            # (self.dfs[key])['time_since_last_faceoff'] = (self.dfs[key]).apply(lambda row : self.time_adjust((self.dfs[key])['time'], (self.dfs[key])['last_faceoff_time']))
            # (self.dfs[key])['truncated_time'] = (self.dfs[key]).apply(lambda row : self.trunc_time_to_intervals((self.dfs[key])['time'], 0.5))
            # DON'T USE (self.dfs[key])['zone'] = (self.dfs[key]).apply(lambda row : self.identifyZone(row), axis=1)
            # print("zone identification complete")
            #self.auditTiming()
            assert (key == initialKey)
            print("zone_changes_since_last_event")
            (self.dfs[key])['zone_changes_since_last_event'] = (self.dfs[key]).apply(
                lambda row: self.zoneChangesSinceLastEvent(key, row), axis=1)
            #print("zone changes since last event identification complete")
            #self.auditTiming()
            assert (key == initialKey)
            print("time_since_faceoff")
            (self.dfs[key])['time_since_faceoff'] = (self.dfs[key]).apply(
                lambda row: self.time_adjust(row['game_seconds'], row['last_faceoff_time']), axis=1)
            #print("time since last faceoff complete")
            #self.auditTiming()
            assert (key == initialKey)
            print("zone_changes_since_faceoff")
            (self.dfs[key])['zone_changes_since_faceoff'] = (self.dfs[key]).apply(
                lambda row: self.findZoneChangesSinceLastFaceoff(key, row), axis=1)
            #print("zone changes since last faceoff complete")
            assert (key == initialKey)
            print("xG_since_faceoff")
            (self.dfs[key])['xG_since_faceoff'] = 0
            (self.dfs[key])['xG_since_faceoff'] = (self.dfs[key]).apply(
                lambda row: self.expectedGoalsSinceFaceoff(key, row), axis=1)
            assert (key == initialKey)
            print("zone time stuff")
            (self.dfs[key])['begin_offensive_zone_time'] = (self.dfs[key]).apply(
                lambda row: self.isStartOfOffensiveZoneTime(key, row), axis=1)
            assert (key == initialKey)
            (self.dfs[key])['end_offensive_zone_time'] = (self.dfs[key]).apply(
                lambda row: self.isEndOfOffensiveZoneTime(key, row), axis=1)
            assert (key == initialKey)
            (self.dfs[key])['begin_defensive_zone_time'] = (self.dfs[key]).apply(
                lambda row: self.isStartOfDefensiveZoneTime(key, row), axis=1)
            assert (key == initialKey)
            (self.dfs[key])['end_defensive_zone_time'] = (self.dfs[key]).apply(
                lambda row: self.isEndOfDefensiveZoneTime(key, row), axis=1)
            assert (key == initialKey)
            (self.dfs[key])['begin_neutral_zone_time'] = (self.dfs[key]).apply(
                lambda row: self.isStartOfNeutralZoneTime(key, row), axis=1)
            assert (key == initialKey)
            (self.dfs[key])['end_neutral_zone_time'] = (self.dfs[key]).apply(
                lambda row: self.isEndOfNeutralZoneTime(key, row), axis=1)
            assert (key == initialKey)
            #(self.dfs[key])['offensive_zone_time'] = 0
            #(self.dfs[key])['defensive_zone_time'] = 0
            #(self.dfs[key])['neutral_zone_time'] = 0
            #assert (key == initialKey)
            # (self.dfs[key])['offensive_zone_time'] = (self.dfs[key]).apply(
            #     lambda row: self.computeOffensiveZoneTime(key, row), axis=1)
            # assert (key == initialKey)
            # (self.dfs[key])['defensive_zone_time'] = (self.dfs[key]).apply(
            #     lambda row: self.computeDefensiveZoneTime(key, row), axis=1)
            # assert (key == initialKey)
            # (self.dfs[key])['neutral_zone_time'] = (self.dfs[key]).apply(
            #    lambda row: self.computeNeutralZoneTime(key, row), axis=1)
            # (self.dfs[key])['offensive_zone_time_PFO'] = (self.dfs[key]).apply(
            #     lambda row: self.computeOffensiveZoneFACTime(key, row), axis=1)
            # assert (key == initialKey)
            # (self.dfs[key])['getToOffensiveZone'] = (self.dfs[key]).apply(
            #     lambda row: self.getToOffensiveZone(key, row), axis=1)
            # assert (key == initialKey)
            # (self.dfs[key])['get_zone_neu_time'] = (self.dfs[key]).apply(
            #     lambda row: self.offensiveZoneNEUTime(key, row), axis=1)
            (self.dfs[key])['currently_offensive_zone_time'] = (self.dfs[key]).apply(
                lambda row: self.isCurrentlyOffensiveZoneTime(key, row), axis=1)
            (self.dfs[key])['year'] = key
            print('more offensive_team')
            (self.dfs[key])['offensive_team'] = np.NaN
            (self.dfs[key])['offensive_team'] = (self.dfs[key]).apply(
                lambda row: self.identifyOffensiveTeam(key, row), axis=1)
            print('more offensive_team')
            (self.dfs[key])['offensive_team'] = (self.dfs[key]).apply(
                lambda row: self.identifyOffensiveTeamForLineChange(key, row), axis=1)
            print('last_faceoff_winnning_team')
            (self.dfs[key])['last_faceoff_winning_team'] = (self.dfs[key]).apply(
                lambda row: self.identifyLastFaceoffWinner(key, row), axis=1)
            print('last_faceoff_winner_on_offense')
            (self.dfs[key])['last_faceoff_winner_on_offense'] = (self.dfs[key]).apply(
                lambda row: self.faceoffWinnerOnOffense(key, row), axis=1)
            print('last_faceoff_winner_on_defense')
            (self.dfs[key])['last_faceoff_winner_on_defense'] = (self.dfs[key]).apply(
                lambda row: self.faceoffWinnerOnDefense(key, row), axis=1)
            (self.dfs[key])['faceoff_winning_team_xG_since_faceoff'] = 0
            (self.dfs[key])['faceoff_losing_team_xG_since_faceoff'] = 0
            print('faceoff_winning_team_xG_since_faceoff')
            (self.dfs[key])['faceoff_winning_team_xG_since_faceoff'] = (self.dfs[key]).apply(
                lambda row: self.faceoffWinningTeamExpectedGoalsSinceFaceoff(key, row), axis=1)
            print('faceoff_losing_team_xG_since_faceoff')
            (self.dfs[key])['faceoff_losing_team_xG_since_faceoff'] = (self.dfs[key]).apply(
                lambda row: self.faceoffLosingTeamExpectedGoalsSinceFaceoff(key, row), axis=1)
            print('last_faceoff_type')
            (self.dfs[key])['last_faceoff_type'] = (self.dfs[key]).apply(
                lambda row: self.lastFaceoffZone(key, row), axis=1)
            print('offense_won_offensive_zone_faceoff')
            (self.dfs[key])['last_event_before_faceoff'] = (self.dfs[key]).apply(
                lambda row: self.isLastEventBeforeFaceoff(key, row), axis=1)
            (self.dfs[key])['offense_team_won_faceoff'] = (self.dfs[key]).apply(
                lambda row: self.offensiveTeamWonFaceoff(key, row), axis=1)
            print('done')
            assert (key == initialKey)
            (self.dfs[key]).to_csv("faceoff_analytics_" + str(key) + ".csv")

    def analyzeOffensePostFaceoff(self):
        analyticsDataFrame = pd.DataFrame(columns=(self.dfs[2015]).columns)
        for key in self.dfs.keys():
            analyticsDataFrame = analyticsDataFrame.append(self.dfs[key], ignore_index=True)
        analyticsDataFrame.to_csv("offense_post_faceoff.csv")
     
    def identifyOffensiveTeam(self, currentYear, row):
        index = row['index']
        if (row['index'] < 0 or row['index'] >= len(self.dfs[currentYear].index)):
            return np.NaN
        if (self.dfs[currentYear].at[index, 'event_zone'] == 'Off'):
            return self.dfs[currentYear].at[index, 'event_team']
        elif (self.dfs[currentYear].at[index, 'event_zone'] == 'Def'):
            currentTeam = self.dfs[currentYear].at[index, 'event_team']
            if currentTeam == self.dfs[currentYear].at[index, 'home_team']:
                return self.dfs[currentYear].at[index, 'away_team']
            else:
                return self.dfs[currentYear].at[index, 'home_team']
        return np.NaN

    def identifyOffensiveTeamForLineChange(self, currentYear, row):
        index = row['index']
        if (row['index'] < 0 or row['index'] >= len(self.dfs[currentYear].index)):
            return np.NaN
        if (self.dfs[currentYear].at[index, 'event_type'] == 'CHANGE'):
            movingIndex = index - 1
            while movingIndex > 0 and (self.dfs[currentYear].at[movingIndex, 'event_type'] == 'CHANGE'):
                movingIndex -= 1
            return self.dfs[currentYear].at[movingIndex, 'offensive_team']
        return self.dfs[currentYear].at[index, 'offensive_team']

    def identifyLastFaceoffWinner(self, currentYear, row):
        index = row['index']
        if (row['index'] < 0 or row['index'] >= len(self.dfs[currentYear].index)):
            return np.NaN
        if self.dfs[currentYear].at[index, 'event_type'] == 'FAC':
            return self.dfs[currentYear].at[index, 'event_team']
        else:
            # Precondition: each df in self.dfs is sorted in oldest-to-newst order of event occurring
            # print(row)
            movingIndex = index
            while (movingIndex > 0 and movingIndex < len(self.dfs[currentYear].index)):
                if self.dfs[currentYear].at[movingIndex, 'event_type'] == 'FAC':
                    return self.dfs[currentYear].at[movingIndex, 'event_team']
                movingIndex -= 1
            return np.NaN

    def isLastEventBeforeFaceoff(self, currentYear, row):
        index = row['index']
        if (row['index'] < 0 or row['index'] >= len(self.dfs[currentYear].index)):
            return np.NaN
        if index == 0 or index == (len(self.dfs[currentYear].index) - 1):
            return False
        return (self.dfs[currentYear].at[index + 1, 'event_type'] == 'FAC')

    def faceoffWinnerOnOffense(self, currentYear, row):
        index = row['index']
        if (row['index'] < 0 or row['index'] >= len(self.dfs[currentYear].index)):
            return np.NaN
        return self.dfs[currentYear].at[index, 'offensive_team'] == self.dfs[currentYear].at[index, 'last_faceoff_winning_team']

    def faceoffWinnerOnDefense(self, currentYear, row):
        index = row['index']
        if (row['index'] < 0 or row['index'] >= len(self.dfs[currentYear].index)):
            return np.NaN
        return self.dfs[currentYear].at[index, 'offensive_team'] != self.dfs[currentYear].at[index, 'last_faceoff_winning_team'] and self.dfs[currentYear].at[index, 'event_zone'] != "Neu"

    def isCurrentlyOffensiveZoneTime(self, currentYear, row):
        index = row['index']
        if (row['index'] < 0 or row['index'] >= len(self.dfs[currentYear].index)):
            return np.NaN
        if index >= len(self.dfs[currentYear].index):
            return np.NaN
        movingIndex = index
        while (movingIndex >= 0 and movingIndex < len(self.dfs[currentYear].index)):
            if (self.dfs[currentYear].at[movingIndex, 'begin_offensive_zone_time'] == True):
                return True
            if (self.dfs[currentYear].at[movingIndex, 'end_offensive_zone_time'] == True):
                return False
            movingIndex -= 1
        return False


    def expectedGoalsSinceFaceoffWithZoneChanges(self, currentYear, row):
        index = row['index']  # slightly shady.. fix later! .. this is a substitute for getting row's index directly
        if (row['index'] < 0 or row['index'] >= len(self.dfs[currentYear].index)):
            return np.NaN
        if index >= len(self.dfs[currentYear].index):
            return np.NaN
        if (self.dfs[currentYear].at[index, 'event_type'] == 'FAC'):
            return 0
        if (self.dfs[currentYear].at[index, 'event_zone'] == 'Neu'):
            return 0
        movingIndex = index
        cumulativeExpectedGoals = 0
        while (movingIndex >= 0 and movingIndex < len(self.dfs[currentYear].index) and self.dfs[currentYear].at[
            movingIndex, 'time_since_faceoff'] > 0):
            currentCumulativeXGoals = self.dfs[currentYear].at[movingIndex, 'xG_since_faceoff']
            if self.dfs[currentYear].at[movingIndex, 'zone_changes_since_last_event'] == 0:
                xGoals = self.dfs[currentYear].at[movingIndex, 'xGoal']
                if (self.dfs[currentYear].at[movingIndex, 'event_type'] == 'GOAL' or self.dfs[currentYear].at[
                    movingIndex, 'event_type'] == 'MISS' or self.dfs[currentYear].at[
                    movingIndex, 'event_type'] == 'SHOT'):
                    if (xGoals > 0):
                        cumulativeExpectedGoals += xGoals
                movingIndex -= 1
            else:
                cumulativeExpectedGoals -= currentCumulativeXGoals
                # Take out any expected goals reflected in current totals that were accumulated outside zone (i.e. before entry to current zone)
                # At this point, currentCumulativeXGoals is cumulative expected goals at most recent past event outside current zone
                break
        return cumulativeExpectedGoals

    def lastFaceoffZone(self, currentYear, row):
        index = row['index']
        if (row['index'] < 0 or row['index'] >= len(self.dfs[currentYear].index)):
            return np.NaN
        if self.dfs[currentYear].at[index, 'event_type'] == 'FAC':
            return self.dfs[currentYear].at[index, 'event_team']
        else:
            # Precondition: each df in self.dfs is sorted in oldest-to-newst order of event occurring
            # print(row)
            movingIndex = index
            while (movingIndex > 0 and movingIndex < len(self.dfs[currentYear].index)):
                if self.dfs[currentYear].at[movingIndex, 'event_type'] == 'FAC':
                    return self.dfs[currentYear].at[movingIndex, 'event_zone']
                movingIndex -= 1
            return np.NaN

    def offensiveTeamWonFaceoff(self, currentYear, row):
        index = row['index']
        if index <= 0 or index >= len(self.dfs[currentYear].index):
            return np.NaN
        if self.dfs[currentYear].at[index, 'event_type'] == 'FAC':
            return np.NaN
        movingIndex = index
        while (movingIndex > 0 and movingIndex < len(self.dfs[currentYear].index)):
            if self.dfs[currentYear].at[movingIndex, 'event_type'] == 'FAC':
                newMovingIndex = movingIndex + 1
                while (newMovingIndex > 0 and newMovingIndex < len(self.dfs[currentYear].index) - 1 and not (self.dfs[currentYear].at[newMovingIndex, 'event_zone'] == 'Off' or self.dfs[currentYear].at[newMovingIndex, 'event_zone'] == 'Neu' or self.dfs[currentYear].at[newMovingIndex, 'event_zone'] == 'Def')):
                    newMovingIndex += 1
                otherMovingIndex = movingIndex
                while (otherMovingIndex > 0 and otherMovingIndex < len(self.dfs[currentYear].index) - 1 and not (self.dfs[currentYear].at[newMovingIndex, 'event_zone'] == 'Off' or self.dfs[currentYear].at[newMovingIndex, 'event_zone'] == 'Neu' or self.dfs[currentYear].at[newMovingIndex, 'event_zone'] == 'Def')):
                    otherMovingIndex -= 1
                return self.dfs[currentYear].at[newMovingIndex, 'offensive_team'] == self.dfs[currentYear].at[otherMovingIndex, 'event_team']
            movingIndex -= 1
        return np.NaN
        # index = row['index']
        # if (self.dfs[currentYear]).at[index, 'last_faceoff_type'] != 'Neu':
        #     if self.dfs[currentYear].at[index, 'event_type'] == 'FAC':
        #         return False
        #     else:
        #         # Precondition: each df in self.dfs is sorted in oldest-to-newst order of event occurring
        #         # print(row)
        #         movingIndex = index
        #         while (movingIndex > 0 and movingIndex < len(self.dfs[currentYear].index)):
        #             if self.dfs[currentYear].at[movingIndex, 'event_type'] == 'FAC':
        #                 break
        #             movingIndex -= 1
        #         # At this point, movingIndex corresponds to last faceoff
        #         if self.dfs[currentYear].at[movingIndex, 'event_team'] == self.dfs[currentYear].at[movingIndex, 'last_faceoff_winner_on_offense']:
        #             return True
        #         return np.NaN
        # else:
        #     return False



def main():
    # sd = SplitData(2015, 2020, True)
    # for year in range(2015, 2020):
    #     print(year)
    #     (sd.dfs[year])['offense_team_won_faceoff'] = (sd.dfs[year]).apply(
    #         lambda row: sd.offensiveTeamWonFaceoff(year, row), axis=1)
    #     sd.dfs[year].to_csv("faceoff_analytics_new_" + str(year) + ".csv")
    #     (sd.dfs[year])['last_event_before_faceoff'] = (sd.dfs[year]).apply(
    #         lambda row: sd.isLastEventBeforeFaceoff(year, row), axis=1)
    # combineDfs()

    # SplitData.start = time.time()
    startYear = 2015
    endYear = 2019 # inclusive
    for year in range(startYear, endYear + 1):
        SplitData.count = 0
        sd = SplitData(year, year+1)
        sd.loadMergedData()
        sd.xG_from_faceoffs()
        #sd.analyzeOffensePostFaceoff()
    sd = SplitData(2012, 2015)
    # sd.splitData()
    # sd.readSplitData()
    sd.loadMergedData()
    sd.xG_from_faceoffs()
    #sd.analyzeOffensePostFaceoff()
    combineDfs()
    # end = time.time()
    # print("overall run time:", str(int(end - SplitData.start)))

    centers = faceoffImpactsByCenter()
    analyzePlayerFaceoffValue(centers)

def faceoffImpactsByCenter():
    # Players
    #   Years
    #       Team
    #       Offensive Zone (5v5)
    #       Defensive Zone (5v5)
    #       Neutral Zone (5v5)
    #       Penalty Kill
    #       Power Play
    #       WITHIN EACH: count, avg, stdev, median, sum, faceoffs
    centers = dict()
    startYear = 2015
    endYear = 2020
    for year in range(startYear, endYear):
        # Read in list of centers
        pbp_df = pd.read_csv("faceoff_analytics_" + str(year) + ".csv")
        print("read play-by-play data")
        print("recording faceoffs")
        for index, row in pbp_df.iterrows():
            if row['event_type'] == 'FAC':
                winningPlayer = row['event_player_1']
                losingPlayer = row['event_player_2']
                for player in [winningPlayer, losingPlayer]:
                    if player not in centers.keys():
                        centers[player] = dict()
                        # Future TODO: add team as dimension here
                    if year not in (centers[player]).keys():
                        (centers[player])[year] = dict()
                        for category in ['offensive_zone', 'defensive_zone', 'neutral_zone', 'penalty_kill', 'power_play']:
                            (((centers[player])[year])[category]) = dict()
                            (((centers[player])[year])[category])['wins'] = dict()
                            (((centers[player])[year])[category])['losses'] = dict()
                            for subcat in ['wins_for', 'wins_against', 'losses_for', 'losses_against']:
                                ((((centers[player])[year])[category])[subcat]) = dict()
                                ((((centers[player])[year])[category])[subcat])['count'] = 0
                                ((((centers[player])[year])[category])[subcat])['avg'] = 0.0
                                ((((centers[player])[year])[category])[subcat])['stdev'] = 0.0
                                ((((centers[player])[year])[category])[subcat])['median'] = 0.0
                                ((((centers[player])[year])[category])[subcat])['sum'] = 0.0
                                ((((centers[player])[year])[category])[subcat])['faceoffs'] = []
                winningPlayer = row['event_player_1']
                losingPlayer = row['event_player_2']
                categorizations = categorizeFaceoffGrouping(row)
                if categorizations == ["other", "other"]:
                    print("empty categorization")
                    categorizations = categorizeFaceoffGrouping(row)
                    continue
                else:
                    print("non-empty")
                winnerCategorization = categorizations[0]
                loserCategorization = categorizations[1]
                movingIndex = row['index'] + 1
                if movingIndex < 0 or movingIndex >= len(pbp_df.index):
                    break
                while movingIndex < (len(pbp_df.index) - 1) and pbp_df.at[movingIndex, 'zone_changes_since_last_event'] == 0 and pbp_df.at[movingIndex, 'event_type'] != 'FAC':
                    movingIndex += 1
                if pbp_df.at[movingIndex, 'event_type'] == 'FAC' and movingIndex > 0:
                    movingIndex -= 1
                newRow = pbp_df.iloc[movingIndex]
                ((((centers[winningPlayer])[year])[winnerCategorization])['wins_for'])['count'] += 1
                ((((centers[winningPlayer])[year])[winnerCategorization])['wins_against'])['count'] += 1
                (((((centers[winningPlayer])[year])[winnerCategorization])['wins_for'])['faceoffs']).append(newRow['faceoff_winning_team_xG_since_faceoff'])
                (((((centers[winningPlayer])[year])[winnerCategorization])['wins_against'])['faceoffs']).append(-1 * newRow['faceoff_losing_team_xG_since_faceoff'])
                ((((centers[winningPlayer])[year])[winnerCategorization])['wins_for'])['sum'] += row['faceoff_winning_team_xG_since_faceoff']
                ((((centers[winningPlayer])[year])[winnerCategorization])['wins_against'])['sum'] += (-1 * newRow['faceoff_losing_team_xG_since_faceoff'])
                ((((centers[losingPlayer])[year])[loserCategorization])['losses_for'])['count'] += 1
                ((((centers[losingPlayer])[year])[loserCategorization])['losses_against'])['count'] += 1
                (((((centers[losingPlayer])[year])[loserCategorization])['losses_for'])['faceoffs']).append(newRow['faceoff_losing_team_xG_since_faceoff'])
                (((((centers[losingPlayer])[year])[loserCategorization])['losses_against'])['faceoffs']).append(-1 * newRow['faceoff_winning_team_xG_since_faceoff'])
                ((((centers[losingPlayer])[year])[loserCategorization])['losses_for'])['sum'] += newRow['faceoff_losing_team_xG_since_faceoff']
                ((((centers[losingPlayer])[year])[loserCategorization])['losses_against'])['sum'] += (-1 * newRow['faceoff_winning_team_xG_since_faceoff'])
    return centers

def analyzePlayerFaceoffValue(centers):
    print("analyzing player faceoff value")
    import statistics
    output_df = pd.DataFrame(columns=['player_year_key', 'player', 'offensive_zone_avg', 'offensive_zone_variability', 'offensive_zone_count', 'defensive_zone_avg', 'defensive_zone_variability', 'defensive_zone_count', 'neutral_zone_avg', 'neutral_zone_variability', 'neutral_zone_count', 'penalty_kill_avg', 'penalty_kill_variability', 'penalty_kill_count', 'power_play_avg', 'power_play_variabilty', 'power_play_count', 'overall_avg', 'overall_variability', 'overall_count', 'positive_value_probability'])
    for player in centers.keys():
        for year in (centers[player]).keys():
            playerYearKey = str(player) + "_" + str(year)
            entry = [playerYearKey, player]
            overall = []
            for cat in ['offensive_zone', 'defensive_zone', 'neutral_zone', 'penalty_kill', 'power_play']:
                # winsForCount = ((((centers[player])[year])[cat])['wins_for'])['count']
                # winsAgainstCount = ((((centers[player])[year])[cat])['wins_against'])['count']
                # winsForSum = ((((centers[player])[year])[cat])['wins_for'])['sum']
                # winsAgainstSum = ((((centers[player])[year])[cat])['wins_against'])['sum']
                combined = (((((centers[player])[year])[cat])['wins_for']))['faceoffs'] + (((((centers[player])[year])[cat])['wins_against'])['faceoffs']) + (((((centers[player])[year])[cat])['losses_for'])['faceoffs']) + (((((centers[player])[year])[cat])['losses_against'])['faceoffs'])
                if len(combined) > 1:
                    avg = statistics.mean(combined)
                    stdev = statistics.stdev(combined)
                    count = len(combined)
                    entry.append(avg)
                    entry.append(stdev)
                    entry.append(count)
                    overall = overall + combined
                else:
                    entry.append(np.NaN)
                    entry.append(np.NaN)
                    entry.append(np.NaN)
            if len(overall) > 1:
                overallAvg = statistics.mean(overall)
                overallStdev = statistics.stdev(overall)
                overallCount = len(overall)
                entry.append(overallAvg)
                entry.append(overallStdev)
                entry.append(overallCount)
                positiveValueCounter = 0
                for i in range(0, len(overall)):
                    if overall[i] > 0:
                        positiveValueCounter += 1
                entry.append(positiveValueCounter / overallCount)
            else:
                entry.append(np.NaN)
                entry.append(np.NaN)
                entry.append(np.NaN)
                entry.append(np.NaN)
            output_df.loc[len(output_df.index)] = entry
            print(playerYearKey)
    print(output_df)
    expectations = dict()
    for cat in ['offensive_zone', 'defensive_zone', 'neutral_zone', 'penalty_kill', 'power_play']:
        output_df[str(cat) + '_total'] = output_df[str(cat) + '_avg'] * output_df[str(cat) + '_count']
        expectations[cat] = (output_df[str(cat) + '_total']).sum(skipna = True) / (output_df[str(cat) + '_count']).sum(skipna = True)
    print("expectations is")
    print(expectations)
    for cat in ['offensive_zone', 'defensive_zone', 'neutral_zone', 'penalty_kill', 'power_play']:
        output_df[str(cat) + '_expected_total'] = expectations[cat] * output_df[str(cat) + '_count']
        output_df[str(cat) + '_vs._expectation'] = output_df[str(cat) + '_total'] - output_df[str(cat) + '_expected_total']
    output_df.to_csv("player_faceoff_value.csv")

def categorizeFaceoffGrouping(row):
    # Returns 2-element array where first element is state of faceoff winner and second element is state of faceoff loser
    if row['game_strength_state'] == '5v5':
        if row['event_zone'] == 'Neu':
            return ["neutral_zone", "neutral_zone"]
        if row['event_zone'] == 'Off' or row['event_zone'] == 'Def':
            faceoffWinningTeam = (str(row['event_description']))[0:3]
            if row['event_team'] == faceoffWinningTeam:
                if row['event_zone'] == 'Off':
                    return ["offensive_zone", "defensive_zone"]
                elif row['event_zone'] == 'Def':
                    return ["defensive_zone", "offensive_zone"]
            else:
                if row['event_zone'] == 'Off':
                    return ["defensive_zone", "offensive_zone"]
                elif row['event_zone'] == 'Def':
                    return ["offensive_zone", "defensive_zone"]
                else:
                    return ["other", "other"]
    else:
        homeSkaters = row['home_skaters']
        awaySkaters = row['away_skaters']
        homeTeam = row['home_team']
        awayTeam = row['away_team']
        faceoffWinningTeam = row['event_team']
        if homeSkaters > awaySkaters and homeTeam == faceoffWinningTeam:
            return ["power_play", "penalty_kill"]
        elif homeSkaters > awaySkaters and awayTeam == faceoffWinningTeam:
            return ["penalty_kill", "power_play"]
        elif homeSkaters < awaySkaters and homeTeam == faceoffWinningTeam:
            return ["penalty_kill", "power_play"]
        elif homeSkaters < awaySkaters and awayTeam == faceoffWinningTeam:
            return ["power_play", "penalty_kill"]
        else:
            return ["other", "other"]

def aggregateShotsData():
    startingYear = 2011
    df = pd.read_csv("offense_post_faceoff_" + str(startingYear) + ".csv")
    shot_related_stats = ['FAC', 'MISS', 'SHOT']
    df['shot_related_stats'] = (df['event_type'] == 'FAC') | (df['event_type'] == 'SHOT') | (df['event_type'] == 'MISS')
    df = df[df['shot_related_stats'] == True]
    print(startingYear)
    aggregateDf = pd.DataFrame(columns=df.columns)
    aggregateDf = aggregateDf.append(df, ignore_index=True)
    for year in range(startingYear + 1, 2021):
        print(year)
        df = pd.read_csv("offense_post_faceoff_" + str(year) + ".csv")
        df['shot_related_stats'] = (df['event_type'] == 'FAC') | (df['event_type'] == 'SHOT') | (df['event_type'] == 'MISS')
        df = df[df['shot_related_stats'] == True]
        aggregateDf = aggregateDf.append(df, ignore_index=True)
    aggregateDf.to_csv("shot_related_offense_post_faceoff.csv")

def combineDfs():
    aggregateDf = pd.read_csv("faceoff_analytics_new_" + str(2015) + ".csv")
    for year in range(2016, 2020):
        print(year)
        df = pd.read_csv("faceoff_analytics_new_" + str(year) + ".csv")
        aggregateDf = aggregateDf.append(df, ignore_index=True)
    aggregateDf.to_csv("faceoff_analytics.csv")

if __name__ == "__main__":
    main()
