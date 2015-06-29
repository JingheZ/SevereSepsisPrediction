__author__ = 'jinghe'
import datetime
import pandas as pd
def readmultiICUInfoData(filename):
    '''Read the patient icu stay info which contains patient id, hospital admit id, hospital seq, hospital admit time,
    hospital discharge time, icustay id, icustay seq, icustay intime, icustay outtime; read the data into dictionaries by patient'''
    f = open(filename, 'rb')
    pts = {}
    i = 0
    for line in f.xreadlines():
        if i > 0:
            line = line.replace('\r\n', '')
            line = line.split(',')
            if not pts.__contains__(line[0]):
                pts[line[0]] = {}
            if not pts[line[0]].__contains__(line[2]):
                pts[line[0]][line[2]] = {}
                # pts[line[0]][line[2]]['admtime'] = line[3]
                # pts[line[0]][line[2]]['distime'] = line[4]
            if not pts[line[0]][line[2]].__contains__(line[6]):
                pts[line[0]][line[2]][line[6]] = {}
                pts[line[0]][line[2]][line[6]]['intime'] = datetime.datetime.strptime(line[7], '"%Y-%m-%d %H:%M:%S"')
                pts[line[0]][line[2]][line[6]]['outtime'] = datetime.datetime.strptime(line[8], '"%Y-%m-%d %H:%M:%S"')
        i += 1
    print('The number of rows: %s', i)
    return pts
#
#
# def readmultiHospInfoData(filename):
#     '''Read the patient icu stay info which contains patient id, hospital admit id, hospital seq, hospital admit time,
#     hospital discharge time, icustay id, icustay seq, icustay intime, icustay outtime; read the data into dictionaries by patient'''
#     f = open(filename, 'rb')
#     pts = {}
#     i = 0
#     for line in f.xreadlines():
#         if i > 0:
#             line = line.replace('\r\n', '')
#             line = line.split(',')
#             if not pts.__contains__(line[0]):
#                 pts[line[0]] = {}
#             if not pts[line[0]].__contains__(line[2]):
#                 pts[line[0]][line[2]] = {}
#                 pts[line[0]][line[2]]['admtime'] = datetime.datetime.strptime(line[3], '"%Y-%m-%d %H:%M:%S"')
#                 pts[line[0]][line[2]]['distime'] = datetime.datetime.strptime(line[4], '"%Y-%m-%d %H:%M:%S"')
#                 # loshosp = pts[line[0]][line[2]]['distime'] - pts[line[0]][line[2]]['admtime']
#             if not pts[line[0]][line[2]].__contains__(line[6]):
#                 pts[line[0]][line[2]][line[6]] = {}
#                 pts[line[0]][line[2]][line[6]]['intime'] = datetime.datetime.strptime(line[7], '"%Y-%m-%d %H:%M:%S"')
#                 pts[line[0]][line[2]][line[6]]['outtime'] = datetime.datetime.strptime(line[8], '"%Y-%m-%d %H:%M:%S"')
#         i += 1
#     print('The number of rows: %s', i)
#
#     for key, value in pts.iteritems():
#         for key1, value1 in value.iteritems():
#             outtimes = []
#             for key2, value2 in value1.iteritems():
#                 outtimes.append(value2['outtime'])
#             if max(outtimes) > value1['distime']:
#                 value1['distime'] = max(outtimes)
#     return pts
#
#


def timeconversion(dt):
    # dt = datetime.datetime.strptime(t0, '"%Y-%m-%d %H:%M:%S"')
    mins = (dt.minute / 30) * 30
    dt1 = dt.replace(minute=mins, second=0)
    return dt1


def findICUSeq(filename, infos):
    '''the bldculture data does not have an icu stay id. hence, we use the chartime of the test and compare it with the
     intime and outtime of an icu stay to determine the icu stay id of the observation'''
    f = open(filename, 'rb')
    i = 0
    icuseqall = []
    unmatch = []
    halfhours = []
    for line in f.xreadlines():
        if i > 0:
            line = line.split(',')
            if len(line[3]) > 2:
                t = datetime.datetime.strptime(line[3], '"%Y-%m-%d %H:%M:%S"')
                t_hr = timeconversion(t)
            else:
                t = line[3]
                t_hr = line[3]
            halfhours.append(t_hr)
            if not infos.__contains__(line[0]):
                icuseq = 0
            elif not infos[line[0]].__contains__(line[2]):
                icuseq = 0
            else:
                icuseq = 0
                if len(line[3]) > 2:
                    for key, value in infos[line[0]][line[2]].iteritems():
                        try:
                            if t >= value['intime'] and t <= value['outtime']:
                                icuseq = int(key)
                                break
                        except ValueError:
                            icuseq = 0
                if icuseq == 0 and len(infos[line[0]][line[2]]) == 1:
                    icuseq = 1
            icuseqall.append(icuseq)
            if icuseq == 0:
                unmatch.append(line)
        i += 1
    print('The number of rows: %s', i)
    return icuseqall, unmatch, halfhours
#
#
# def findhospSeq(filename, infos):
#     '''the bldculture data does not have an icu stay id. hence, we use the chartime of the test and compare it with the
#      intime and outtime of an icu stay to determine the icu stay id of the observation'''
#     f = open(filename, 'rb')
#     i = 0
#     hospseqall = []
#     for line in f.xreadlines():
#         if i > 0:
#             line = line.split(',')
#             if not infos.__contains__(line[0]):
#                 seq = 1
#             else:
#                 seq = 0
#                 for key, value in infos[line[0]].iteritems():
#                     try:
#                         t = datetime.datetime.strptime(line[3], '"%Y-%m-%d %H:%M:%S"')
#                         if t >= value['admtime'] and t <= value['distime']:
#                             seq = int(key)
#                             break
#                     except ValueError:
#                         continue
#             hospseqall.append(seq)
#         i += 1
#     print('The number of rows: %s', i)
#     return hospseqall
#
#
#
#
pts_hadmicu = readmultiICUInfoData('ptsicustayinfo.csv')
icuseqs, unmatch, halfhours = findICUSeq('pts_bldcultures_new.csv', pts_hadmicu)

a = 0
for i in icuseqs:
    if i == 0:
        a += 1
print a #7892

ap = set() #unique patients in the unmatch data
missingtime = 0 #number of lines with missing time stamp
ap_missing = set()
for i in unmatch:
    ap.add(i[0]) #1023 patients
    if i[3] == '""':
        missingtime += 1
        ap_missing.add(i[0]) #506 patients
print missingtime #1064

icuseqs_df = pd.DataFrame(icuseqs)
icuseqs_df.to_csv('icuseqs.csv', sep=',', index=False, header=False)

halfhours_df = pd.DataFrame(halfhours)
halfhours_df.to_csv('halfhours.csv', sep=',', index=False, header=False)


def findICUSeq2(filename, infos):
    '''the bldculture data does not have an icu stay id. hence, we use the chartime of the test and compare it with the
     intime and outtime of an icu stay to determine the icu stay id of the observation'''
    f = open(filename, 'rb')
    i = 0
    icuseqall = []
    unmatch = []
    halfhours = []
    for line in f.xreadlines():
        if i > 0:
            line = line.split(',')
            if len(line[3]) > 2:
                t = datetime.datetime.strptime(line[3], '"%Y-%m-%d %H:%M:%S"')
                t_hr = timeconversion(t)
            else:
                t = line[3]
                t_hr = line[3]
            halfhours.append(t_hr)
            if not infos.__contains__(line[0]):
                icuseq = 0
            elif not infos[line[0]].__contains__(line[2]):
                icuseq = 0
            else:
                icuseq = 0
                if len(line[3]) > 2:
                    for key, value in infos[line[0]][line[2]].iteritems():
                        try:
                            if t >= value['intime'] and t <= value['outtime']:
                                icuseq = int(key)
                                break
                        except ValueError:
                            icuseq = 0
                if icuseq == 0:
                    if len(infos[line[0]][line[2]]) == 1:
                        icuseq = 1
                    elif len(line[3]) > 2:
                        gaptime = {}
                        for key, value in infos[line[0]][line[2]].iteritems():
                            ingap = max(t, value['intime']) - min(t, value['intime'])
                            outgap = max(t, value['outtime']) - min(t, value['outtime'])
                            gaptime[key] = min(ingap, outgap)
                        mingap = min(gaptime, key=gaptime.get)
                        if gaptime[mingap] < datetime.timedelta(0, 86400):
                            icuseq = int(mingap)
            icuseqall.append(icuseq)
            if icuseq == 0:
                unmatch.append(line)
        i += 1
    print('The number of rows: %s', i)
    return icuseqall, unmatch, halfhours

icuseqs, unmatch, halfhours = findICUSeq2('pts_bldcultures_new.csv', pts_hadmicu)


a = 0
for i in icuseqs:
    if i == 0:
        a += 1
print a #5646

ap = set()
missingtime = 0
ap_missing = set()
for i in unmatch:
    ap.add(i[0]) #839 patients
    if i[3] == '""':
        missingtime += 1
        ap_missing.add(i[0]) #506 patients
print missingtime #1064


icuseqs_df = pd.DataFrame(icuseqs)
icuseqs_df.to_csv('icuseqs.csv', sep=',', index=False, header=False)

halfhours_df = pd.DataFrame(halfhours)
halfhours_df.to_csv('halfhours.csv', sep=',', index=False, header=False)
# pts_multihospital, diff = readmultiHospInfoData('multi_hospitaladmit.csv')
# hospseqs = findhospSeq('pts_vitals_new.csv', pts_multihospital)
# hospseqs_df = pd.DataFrame(hospseqs)
# hospseqs_df.to_csv('hospseqs.csv', sep=',', index=False, header=False)




def readmultiHospInfoData(filename):
    '''Read the patient icu stay info which contains patient id, hospital admit id, hospital seq, hospital admit time,
    hospital discharge time, icustay id, icustay seq, icustay intime, icustay outtime; read the data into dictionaries by patient'''
    f = open(filename, 'rb')
    pts = {}
    i = 0
    for line in f.xreadlines():
        if i > 0:
            line = line.replace('\r\n', '')
            line = line.split(',')
            if not pts.__contains__(line[0]):
                pts[line[0]] = {}
            if not pts[line[0]].__contains__(line[2]):
                pts[line[0]][line[2]] = set()
            pts[line[0]][line[2]].add(line[5])
                # pts[line[0]][line[2]]['admtime'] = datetime.datetime.strptime(line[3], '"%Y-%m-%d %H:%M:%S"')
                # pts[line[0]][line[2]]['distime'] = datetime.datetime.strptime(line[4], '"%Y-%m-%d %H:%M:%S"')
        i += 1
    print('The number of rows: %s', i)
    return pts


def getcomorb(filename):
    f = open(filename, 'rb')
    comorb = {}
    i = 0
    for line in f.xreadlines():
        if i > 0:
            line = line.split(',')
            for i in range(len(line)):
                if len(line[i]) < 1:
                    line[i] = 'NA'
            if not comorb.__contains__(line[0]):
                comorb[line[0]] = {}
            if not comorb[line[0]].__contains__(line[2]):
                comorb[line[0]][line[2]] = [line[3], line[9], line[11], line[13], line[15], line[20]]
        i += 1
    print('The number of rows: %s', i)
    return comorb


def findhospSeqComorb(filename, infos, comorbinfo):
    '''the bldculture data does not have an icu stay id. hence, we use the chartime of the test and compare it with the
     intime and outtime of an icu stay to determine the icu stay id of the observation'''
    f = open(filename, 'rb')
    i = 0
    hospseqall = []
    chf = []
    paralysis = []
    pulmonary = []
    diabetes = []
    renal = []
    cancer = []
    for line in f.xreadlines():
        if i > 0:
            line = line.split(',')
            if not infos.__contains__(line[0]):
                seq = 1
            else:
                seq = 0
                for key, value in infos[line[0]].iteritems():
                    if line[1] in value:
                        seq = int(key)
                        break
            if seq == 0:
                print line
            hospseqall.append(seq)
            seq = str(seq)
            chf.append(comorbinfo[line[0]][seq][0])
            paralysis.append(comorbinfo[line[0]][seq][1])
            pulmonary.append(comorbinfo[line[0]][seq][2])
            diabetes.append(comorbinfo[line[0]][seq][3])
            renal.append(comorbinfo[line[0]][seq][4])
            cancer.append(comorbinfo[line[0]][seq][5])
        i += 1
    print('The number of rows: %s', i)
    return hospseqall, [chf, paralysis, pulmonary, diabetes, renal, cancer]


def comorb(filename, comorbinfo, hseq):
    f = open(filename, 'rb')
    i = 0
    chf = []
    paralysis = []
    pulmonary = []
    diabetes = []
    renal = []
    cancer = []
    for line in f.xreadlines():
        if i > 0:
            line = line.split(',')
            chf.append(comorbinfo[line[0]][line[hseq]][0])
            paralysis.append(comorbinfo[line[0]][line[hseq]][1])
            pulmonary.append(comorbinfo[line[0]][line[hseq]][2])
            diabetes.append(comorbinfo[line[0]][line[hseq]][3])
            renal.append(comorbinfo[line[0]][line[hseq]][4])
            cancer.append(comorbinfo[line[0]][line[hseq]][5])
        i += 1
    print('The number of rows: %s', i)
    return [chf, paralysis, pulmonary, diabetes, renal, cancer]



def getinfecdysfunc(filename):
    f = open(filename, 'rb')
    icds = {}
    i = 0
    for line in f.xreadlines():
        if i > 0:
            line = line.split(',')
            if len(i[2]) > 0:
                if not icds.__contains__(line[0]):
                    icds[line[0]] = {}
                if not icds[line[0]].__contains__(line[1]):
                    icds[line[0]][line[1]] = 1
        i += 1
    print('The number of rows: %s', i)
    return icds


# def icdcodes(filename, icds):
#     f = open(filename, 'rb')
#     i = 0
#     infectdysfunc = []
#     for line in f.xreadlines():
#         if i > 0:
#             line = line.split(',')
#             if icds.__contains__(i[0])
#
#         i += 1
#     print('The number of rows: %s', i)
#     return [chf, paralysis, pulmonary, diabetes, renal, cancer]




pts_multihospital = readmultiHospInfoData('multi_hospitaladmit.csv')
comorbinfo = getcomorb('pts_comorbids_new.csv')
hospseqs, comorbs_vitals = findhospSeqComorb('pts_vitals2_new.csv', pts_multihospital, comorbinfo)

bldcomorbs = comorb('pts_bldcultures2_new.csv', comorbinfo, 2)
labcomorbs = comorb('pts_lab2_new.csv', comorbinfo, 3)


def write(data, filename, filename2):
    for d in range(len(data)):
        df = pd.DataFrame(data[d])
        df.to_csv(str(filename[d])+str(filename2)+'.csv', sep=',', index=False, header=False)

write(labcomorbs, ['chf', 'paralysis', 'pulmonary', 'diabetes', 'renal', 'cancer'], '_labs')
write(bldcomorbs, ['chf', 'paralysis', 'pulmonary', 'diabetes', 'renal', 'cancer'], '_blds')
write(comorbs_vitals, ['chf', 'paralysis', 'pulmonary', 'diabetes', 'renal', 'cancer'], '_vitals')


#deal with the infectious codes
scode = "001, Cholera; 002, Typhoid/paratyphoid fever; 003, Other salmonella infection; 004, Shigellosis; \
005, Other foodpoisoning; 008, Intestinal infection nototherwise classified; 009, Ill-defined intestinalinfection; \
010, Primary tuberculosisinfection; 011, Pulmonary tuberculosis; 012, Other respiratory tuberculosis; \
013, Central nervous system tuberculosis; 014, Intestinal tuberculosis; 015, Tuberculosisof bone and joint; \
016, Genitourinarytuberculosis; 017, Tuberculosisnot otherwise classified; 018, Miliary tuberculosis; 020, Plague; \
021, Tularemia; 022, Anthrax; 023, Brucellosis; 024, Glanders; 025, Melioidosis; 026, Rat-bite fever; 027, Other bacterial zoonoses; \
030, Leprosy; 031, Other mycobacterial disease; \
032, Diphtheria; 033, Whooping cough; \
034, Streptococcal throat/scarlet fever; \
035, Erysipelas; 036, Meningococcal infection; \
037, Tetanus; 038, Septicemia; \
039, Actinomycotic infections; 040, Other\
bacterial diseases; 041, Bacterial infection\
in other diseases not otherwise specified; \
090, Congenital syphilis; 091, Early\
symptomatic syphilis; 092, Early syphilis\
latent; 093, Cardiovascular syphilis; 094,\
Neurosyphilis; 095, Other late symptomatic\
syphilis; 096, Late syphilis latent; \
097, Other and unspecified syphilis; 098,\
Gonococcal infections; 100, Leptospirosis; \
101, Vincent’s angina; 102, Yaws; 103,\
Pinta; 104, Other spirochetal infection; \
110, Dermatophytosis; 111, Dermatomycosis\
not otherwise classified or specified; \
112, Candidiasis; 114, Coccidioidomycosis; \
115, Histoplasmosis; 116, Blastomycotic\
infection; 117, Other mycoses; 118,\
Opportunistic mycoses; 320, Bacterial\
meningitis; 322, Meningitis, unspecified; \
324, Central nervous system abscess; 325,\
Phlebitis of intracranial sinus; 420, Acute\
pericarditis; 421, Acute or subacute endocarditis; \
451, Thrombophlebitis; 461,\
Acute sinusitis; 462, Acute pharyngitis; \
463, Acute tonsillitis; 464, Acute laryngitis/\
tracheitis; 465, Acute upper respiratory\
infection of multiple sites/not otherwise\
specified; 481, Pneumococcal\
pneumonia; 482, Other bacterial pneumonia; \
485, Bronchopneumonia with organism\
not otherwise specified; 486,\
Pneumonia, organism not otherwise\
specified; 491.21, Acute exacerbation of\
obstructive chronic bronchitis; 494,\
Bronchiectasis; 510, Empyema; 513,\
Lung/mediastinum abscess; 540, Acute\
appendicitis; 541, Appendicitis not otherwise\
specified; 542, Other appendicitis; \
562.01, Diverticulitis of small intestine\
without hemorrhage; 562.03, Diverticulitis\
of small intestine with hemorrhage; \
562.11, Diverticulitis of colon without\
hemorrhage; 562.13, Diverticulitis of colon\
with hemorrhage; 566, Anal and rectal\
abscess; 567, Peritonitis; 569.5, Intestinal\
abscess; 569.83, Perforation of\
intestine; 572.0, Abscess of liver; 572.1,\
Portal pyemia; 575.0, Acute cholecystitis; \
590, Kidney infection; 597, Urethritis/\
urethral syndrome; 599.0, Urinary tract\
infection not otherwise specified; 601,\
Prostatic inflammation; 614, Female pelvic\
inflammation disease; 615, Uterine inflammatory\
disease; 616, Other female\
genital inflammation; 681, Cellulitis, finger/\
toe; 682, Other cellulitis or abscess; \
683, Acute lymphadenitis; 686, Other local\
skin infection; 711.0, Pyogenic arthritis; \
730, Osteomyelitis; 790.7, Bacteremia; \
996.6, Infection or inflammation of\
device/graft; 998.5, Postoperative infection; \
999.3, Infectious complication of\
medical care not otherwise classified"

s1 = scode.split('; ')
s0 = []
for i in s1:
    s0.append(i.split(',')[0])
saproxm = []
for i in s0:
    i0 = i.replace('.', '')
    if len(i0) < 5:
        i = i + '%'
    saproxm.append(i)

print saproxm

