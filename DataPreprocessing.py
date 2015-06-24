__author__ = 'jinghe'
import datetime
import pandas as pd
# def readmultiICUInfoData(filename):
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
#                 # pts[line[0]][line[2]]['admtime'] = line[3]
#                 # pts[line[0]][line[2]]['distime'] = line[4]
#             if not pts[line[0]][line[2]].__contains__(line[6]):
#                 pts[line[0]][line[2]][line[6]] = {}
#                 pts[line[0]][line[2]][line[6]]['intime'] = datetime.datetime.strptime(line[7], '"%Y-%m-%d %H:%M:%S"')
#                 pts[line[0]][line[2]][line[6]]['outtime'] = datetime.datetime.strptime(line[8], '"%Y-%m-%d %H:%M:%S"')
#         i += 1
#     print('The number of rows: %s', i)
#     return pts
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
# def findICUSeq(filename, infos):
#     '''the bldculture data does not have an icu stay id. hence, we use the chartime of the test and compare it with the
#      intime and outtime of an icu stay to determine the icu stay id of the observation'''
#     f = open(filename, 'rb')
#     i = 0
#     icuseqall = []
#     for line in f.xreadlines():
#         if i > 0:
#             line = line.split(',')
#             if not infos.__contains__(line[0]):
#                 icuseq = 1
#             elif not infos[line[0]].__contains__(line[2]):
#                 icuseq = 1
#             else:
#                 icuseq = 0
#                 for key, value in infos[line[0]][line[2]].iteritems():
#                     try:
#                         t = datetime.datetime.strptime(line[3], '"%Y-%m-%d %H:%M:%S"')
#                         if t >= value['intime'] and t <= value['outtime']:
#                             icuseq = int(key)
#                             break
#                     except ValueError:
#                         continue
#             icuseqall.append(icuseq)
#         i += 1
#     print('The number of rows: %s', i)
#     return icuseqall
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
# pts_multiicu = readmultiICUInfoData('multi_ICUstays.csv')
# icuseqs = findICUSeq('pts_bldcultures_new.csv', pts_multiicu)
# icuseqs_df = pd.DataFrame(icuseqs)
# icuseqs_df.to_csv('icuseqs.csv', sep=',', index=False, header=False)
#
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

