var hourNumber = (day, hour) => (day * 10) + (hour - 7);
    
var monday = hour => hourNumber(0, hour);
var tuesday = hour => hourNumber(1, hour);
var wednesday = hour => hourNumber(2, hour);
var thursday = hour => hourNumber(3, hour);
var friday = hour => hourNumber(4, hour);
var excludedTimes = [];

function processFree(free) {
    var fns = {
        'Monday': monday,
        'Tuesday': tuesday,
        'Wednesday': wednesday,
        'Thursday': thursday,
        'Friday': friday
    };
    var processed = [];
    free.forEach(freeTime => 
        processed.push(fns[freeTime.day](freeTime.time))
    );

    return processed;
}

function dayOfGroup(groupNum){
    var dayNum = Math.floor(groupNum/10);
    switch (dayNum) {
        case 0:
            return "Monday";
        case 1:
            return "Tuesday";
        case 2:
            return "Wednesday";
        case 3:
            return "Thursday";
        default:
            return "Friday";
    }
}

function hourOfGroup(groupNum) {
    return groupNum - Math.floor(groupNum/10)*10 + 7;
}

function loadData(people) {
    /* reformats the data from the file,
     and creates a leaders and a members object from it, like:
    var leaders = [
        {name: "Alison", gender: 'f', times: [monday(8), monday(9), monday(10), monday (15)]},
        {name: "Claire", gender: 'f', times: [monday(9),monday(10)]}];
    var members = [
        {name: "Pat", gender: 'm', times: [wednesday(11), tuesday(9)]},
        {name: "Eloise", gender: 'f', times: [monday(9),monday(10)]}];
    */

    var leaders = [];
    var members = [];
    for (var x in people) {
        if (people.hasOwnProperty(x)) {
            var person = people[x];
            var name = person.name;
            var role = person.role;
            var gender = person.gender === "Male" ? "m" : "f";
            var times = processFree(person.free);
            if (role === "Leader") {
                leaders.push({id: x, name: name, gender: gender, times: times});
            } else if (role === "Member") {
                members.push({id: x, name: name, gender: gender, times: times});
            }
        }
    }

    return {leaders: leaders, members: members};
}

function setComboDetails(groupCombo) {
    var groups = {};
    var hash = 0;
    if (groupCombo.length !== 0) { 
        groupCombo.forEach(group => {
            hash++;
            var groupDetails = {};
            var groupTimeDetails = {};
            groupTimeDetails.day = dayOfGroup(group.time);
            groupTimeDetails.time = hourOfGroup(group.time);
            groupDetails.time = groupTimeDetails;
            groupDetails.people = group.members;
            groups[hash] = groupDetails;
        });
    }
    return groups;
}

function setOptionList(options) {
    var listOfOptions = {};
    var hash = 0;
    if (options.length !== 0) {
        options.forEach(groupCombo => {
            hash++;
            var groups = setComboDetails(groupCombo);
            listOfOptions[hash] = {groups: groups, rank: groupCombo.rank};
        });
    }
    return listOfOptions;
}

function rankTimes(peopleList) {
    /* form an array where the value at each index
    is the number of occurances of that free time in 
    peopleList */
    var timeRanks = [];
    for (var i = 0; i < peopleList.length; i++) {
        var times = peopleList[i].times;
        for (var j = 0; j< times.length; j++) {
            var t = times[j];
            timeRanks[t] = timeRanks[t] ? timeRanks[t] + 1 : 1;
        }
    }
    return timeRanks;
}

function addConvenienceRank(peopleList) {
    /* gives a rating for how free a leader or member is,
    weighted by which hours others are free for - so that the
    people with the most inconvenient timetables can be 
    dealt with first */

    // first find which times are least popular
    var timeRanks = rankTimes(peopleList);

    // then give each person a rank based on how popular their times are
    for (var i = 0; i < peopleList.length; i++) {
        var times = peopleList[i].times;
        var personRank = 0;
        for (var j = 0; j< times.length; j++) {
            personRank += timeRanks[times[j]];
        }
        peopleList[i].rank = personRank;
    }
    return peopleList;
}

function sortPeopleByRank(peopleList) {
    peopleList = addConvenienceRank(peopleList);
    peopleList.sort(function(a,b) {
        return a.rank - b.rank;
    });
    return peopleList;
}

function intersect(a, b) {
    var t;
    if (b.length > a.length) {
        t = b;
        b = a;
        a = t;
    }
    return a.filter(e => b.indexOf(e) > -1);
}

function invertTimes(times) {
    var inverse = [];
    for (var i=1; i<51; i++) {
        if (times.indexOf(i) === -1) {
            inverse.push(i);
        }
    }
    return inverse;
}

function remove(a, b) {
    return b.filter(e => e !== a);
}

function findLeaderCombos(leaderList, n, mixed) {
    /* return a list of all sets of n times for which leader
    pairs can be found */
    var leader1 = leaderList[0];
    var otherLeaders = leaderList.slice(1);
    var result = [];
    var sigs = [];
    var allowableTimes = invertTimes(excludedTimes);

    for (var i = 0; i < otherLeaders.length; i++) {
        var leader2 = otherLeaders[i];
        var times1 = leader1.times;
        var times2 = leader2.times;
        var commonTimes = intersect(intersect(times1, times2), allowableTimes);
        var groups = [];
        var j;
        if (mixed === 'y' || leader1.gender !== leader2.gender) {
            for (j = 0; j < commonTimes.length; j++) {
                groups.push({gender: 'b', time: commonTimes[j]});
            }
        } else {    
            for (j = 0; j < commonTimes.length; j++) {
                groups.push({gender: leader1.gender, time: commonTimes[j]});
            }
        }
        var newLeaders = remove(leader2, otherLeaders);
        var otherResults;
        if (n > 1) {
            otherResults = findLeaderCombos(newLeaders, n-1, mixed);
        } else {
            otherResults = [[]];
        }

        for (var k = 0; k < otherResults.length; k++) {
            for (var l = 0; l < groups.length; l++) {
                var val = [groups[l]].concat(otherResults[k]);
                var sig = JSON.stringify(val);
                if (sigs.indexOf(sig) === -1) {
                    result.push(val);
                    sigs.push(sig);
                }
            } 
        }
    }
    return result;
}

function fitsInGroup(person, group) {
    var fits = false;
    if (person.times.indexOf(group.time) >= 0 &&
        (group.gender === 'b' ||
        group.gender === person.gender)) {
        fits = true;
    }
    return fits;
}

function sgSolver(members, leaders, leaderTimes, maxLeftOut) {
    var everyone = members.concat(leaders);
    var options = [];

    //for each combo
    for (var i = 0; i < leaderTimes.length; i++) {
        var combo = leaderTimes[i];
        var leftOut = 0;
        var tooManyLeftOut = false;

        // for each person
        for (var j = 0; j < everyone.length; j++) {
            var person = everyone[j];
            var fits = false;
            // go through every group in the combo 
            // and see if person fits in any
            for (var k = 0; k < combo.length; k++) {
                var group = combo[k];
                if (fitsInGroup(person, group)) {
                    fits = true;
                    break;
                }
            }
            if (!fits) {
                leftOut++;
            }
            if (leftOut > maxLeftOut) {
                tooManyLeftOut = true;
                break;
            }

        }
        if (!tooManyLeftOut) {
            options.push(combo);
        }
    }

    return options;
}

function comboTimeRank(groupCombo) {
    var justTimes = groupCombo.map(e => e.time);
    var k = 0;

    // gives each time a rank from 1 to 10 based on handiness.
    // 10 is worst, 1 is best.
    const ranks = {
        10: [tuesday(13), wednesday(13), thursday(13)],
        9: [monday(8), tuesday(8), wednesday(8), thursday(8), friday(8)],
        8: [monday(17), tuesday(17), wednesday(17), thursday(17), friday(17)],
        7: [monday(16), tuesday(16), wednesday(16), thursday(16), friday(16)],
        6: [monday(9), tuesday(9), wednesday(9), thursday(9), friday(9)],
        5: [monday(15), tuesday(15), wednesday(15), thursday(15), friday(15)],
        4: [monday(10), tuesday(10), wednesday(10), thursday(10), friday(10)],
        3: [monday(14), tuesday(14), wednesday(14), thursday(14), friday(14)],
        2: [monday(11), tuesday(11), wednesday(11), thursday(11), friday(11)],
        1: [monday(12), monday(13), tuesday(12), wednesday(12), thursday(12), friday(12), friday(13)]
    };

    justTimes.forEach(time => {
        for (var n=1; n<11; n++) {
            if (ranks[n].indexOf(time) > -1) {
                k += (n * 1000);
                break;
            }
        }
    });
    return k;
}

function splitIntoGenders(members) {
    var maleMembers = [];
    var femaleMembers = [];

    members.forEach(person => {
        if (person.gender === 'm') {
            maleMembers.push(person);
        } else {
            femaleMembers.push(person);
        }
    });

    return {maleMembers: maleMembers, femaleMembers: femaleMembers};
}

function comboTotalRank(groupCombo, members) {
    var genderSplitMembers = splitIntoGenders(members);
    var maleMembers = genderSplitMembers.maleMembers;
    var femaleMembers = genderSplitMembers.femaleMembers;

    var numGroups = groupCombo ? groupCombo.length : 0;
    var bCount = 0;
    var mCount= 0
    var fCount = 0;
    
    groupCombo.forEach(e => {
        (e.gender === 'f' ? fCount++ : (e.gender === 'm' ? mCount++ : bCount++));
    });
    
    if (bCount > 0) {
        var idealM = (maleMembers.length - (mCount * members.length / numGroups)) / bCount;
        var idealF = (femaleMembers.length - (fCount * members.length / numGroups)) / bCount;
    }
    var idealSize = members.length / numGroups;
    var peopleRank = 0;

    var mRank;
    var fRank;  
    var sizeRank;
    groupCombo.forEach(group => {
        sizeRank = 0;
        mRank = 0;
        fRank = 0;

        maleMembers.forEach(person => {
            if (person.times.indexOf(group.time) > -1) {
                mRank += 1 / groupsAvailableToPerson(groupCombo, person);
            }            
        });
        femaleMembers.forEach(person => {
            if (person.times.indexOf(group.time) > -1) {
                fRank += 1 / groupsAvailableToPerson(groupCombo, person);
            }            
        });
        sizeRank = mRank + fRank;
        if(group.gender === 'b') {
            peopleRank += 1000 * ((idealSize - sizeRank)*(idealSize - sizeRank) + (idealM - mRank)*(idealM - mRank) + (idealF - fRank)*(idealF - fRank));
        } else {
            peopleRank += 1000 * ((idealSize - sizeRank)*(idealSize - sizeRank));
        }
    });

    peopleRank += 1000 * Math.min(mCount, fCount);
    peopleRank += comboTimeRank(groupCombo);
    return peopleRank;

}

function runSolvers(data, numGroups, mixed, maxLeftOut) {
    var people = loadData(data);
    people.leaders = sortPeopleByRank(people.leaders);
    people.members = sortPeopleByRank(people.members);
    var leaderCombos = findLeaderCombos(people.leaders, numGroups, mixed);
    var allCombos = sgSolver(people.members, people.leaders, leaderCombos, maxLeftOut);
    allCombos.forEach(combo => {
        combo.rank = comboTotalRank(combo, people.members);
        combo = formGroups(combo, people.leaders, people.members);
    });

    allCombos.sort(function(a,b) {
        return a.rank - b.rank;
    });

    return allCombos;
}

function groupsAvailableToPerson(groupCombo, person) {
    var k = 0;
    groupCombo.forEach(group => {
        if (fitsInGroup(person, group)) {
            k++;
        }
    }); 
    return k;
}

function peopleAvailableToGroup(group, people) {
    var k = 0;
    people.forEach(person => {
        if (fitsInGroup(person, group)) {
            k++;
        }
    }); 
    return k;
}

function formGroups(groupCombo, leaders, members, mixed) {
    var genderSplitLeaders, maleLeaders, femaleLeaders;
    if (mixed === 'n') {
        genderSplitLeaders = splitIntoGenders(leaders);
        maleLeaders = genderSplitLeaders.maleMembers;
        femaleLeaders = genderSplitLeaders.femaleMembers;
    }
    groupCombo.forEach(group => {
        var availableLeaders = peopleAvailableToGroup(group, leaders);
        var availableMaleLeaders, availableFemaleLeaders;
        if (mixed === 'n') {
            availableMaleLeaders = peopleAvailableToGroup(group, maleLeaders);
            availableFemaleLeaders = peopleAvailableToGroup(group, femaleLeaders); 
        }

        group.members = [];
        leaders.forEach(person => {
            if ((availableLeaders === 2 ||
                (mixed === 'n' && group.gender === 'b' &&
                    ((availableMaleLeaders === 1 && person.gender === 'm') || 
                    (availableFemaleLeaders === 1 && person.gender === 'f'))) ||
                (groupsAvailableToPerson(groupCombo, person) === 1) && 
                fitsInGroup(person, group))) {
                group.members.push(person.id);
            }
        });
        members.forEach(person => {
            if (groupsAvailableToPerson(groupCombo, person) === 1 && fitsInGroup(person, group)) {
                group.members.push(person.id);
            }
        });
    });
    return groupCombo;
}
