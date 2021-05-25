from __future__ import division
import sys, os, numpy, subprocess, multiprocessing, time, gc
import matplotlib.pyplot as plt
import pickle
import scipy.stats
from sklearn.neighbors import KernelDensity
import functools
import matplotlib.pyplot as plt

def ReadPrograms(filename,sep=';', yr='2020'):
    f = open(filename, 'r')
    lines = f.readlines()
    programs = {}
    ct = 0
    for line in lines:
        line = line.strip().rstrip('\r')
        pieces = line.split(sep)
        if 'CODIGO' in pieces[0]:
            continue
        id_c = int(pieces[0])
        if id_c not in programs:
            programs[id_c] = {}

        programs[id_c]['nem'] = int(pieces[3]) if pieces[3].isdigit() else 0
        programs[id_c]['rank'] = int(pieces[4]) if pieces[4].isdigit() else 0
        programs[id_c]['lenguaje'] = int(pieces[5]) if pieces[5].isdigit() else 0
        programs[id_c]['matematica'] = int(pieces[6]) if pieces[6].isdigit() else 0
        programs[id_c]['historia'] = int(pieces[7]) if pieces[7].isdigit() else 0
        programs[id_c]['ciencias'] = int(pieces[8]) if pieces[8].isdigit() else 0
        programs[id_c]['promedio_minimo'] = int(pieces[9]) if pieces[9].isdigit() else 0
        programs[id_c]['ponderado_minimo'] = int(pieces[10])*100 if pieces[10].isdigit() else 0
        programs[id_c]['ponderado_minimo_antes_pe_pond'] = int(pieces[11])*100 if pieces[11].isdigit() else 0
        programs[id_c]['vacantes_reg'] = 0
        for r in range(12, 16):
            seats = int(pieces[r]) if pieces[r].isdigit() else 0
            programs[id_c]['vacantes_reg'] = programs[id_c]['vacantes_reg'] + seats
        programs[id_c]['restringe_sexo'] = False
        if 'S' in pieces[16]:
            programs[id_c]['restringe_sexo'] = True

        programs[id_c]['prueba_especial'] = False
        programs[id_c]['pct_prueba_especial'] = 0
        programs[id_c]['tipo_prueba_especial'] = None
        if 'S' in pieces[17]:
            programs[id_c]['prueba_especial'] = True
            programs[id_c]['tipo_prueba_especial'] = pieces[18]
            programs[id_c]['pct_prueba_especial'] = int(pieces[19]) if pieces[19].isdigit() else 0

        programs[id_c]['prueba_electiva'] = False
        if 'S' in pieces[20]:
	        programs[id_c]['prueba_electiva'] = True

        if yr in ['2019', '2020']:
            programs[id_c]['excluye_desde_pref'] = int(pieces[21]) if pieces[21].isdigit() else 0
            programs[id_c]['max_post'] = int(pieces[21])-1 if pieces[21].isdigit() else 0
            programs[id_c]['vacantes_bea'] = int(pieces[22]) if pieces[22].isdigit() else 0
            programs[id_c]['excluye_egresado_antes'] = int(pieces[23]) if pieces[23].isdigit() else 0
            if 'S' in pieces[24]:
                programs[id_c]['pedagogia'] = True
            programs[id_c]['min_ranking'] = int(pieces[25]) if pieces[25].isdigit() else 0
        elif yr == '2021':
            programs[id_c]['max_post'] = int(pieces[21]) if pieces[21].isdigit() else 0
            programs[id_c]['excluye_desde_pref'] = int(pieces[22]) if pieces[22].isdigit() else 0
            programs[id_c]['vacantes_bea'] = int(pieces[23]) if pieces[23].isdigit() else 0
            programs[id_c]['excluye_egresado_antes'] = int(pieces[24]) if pieces[24].isdigit() else 0
            if 'S' in pieces[25]:
                programs[id_c]['pedagogia'] = True
            programs[id_c]['min_ranking'] = int(pieces[26]) if pieces[26].isdigit() else 0
        else:
            print("ERROR: Unkonwn year. ")
            sys.exit(1)
    f.close()
    return programs

def ReadApplications(filename, col_id=0, col_post=3, info_bea=False, yr='2021', sep=';'):
    stime = time.time()
    f = open(filename, 'r')
    lines = f.readlines()
    students = {}
    ct = 0
    print 'Storing applications of students'
    for line in lines:
        if ct%10000 == 0:
            print '    Students processed:', ct
        ct+=1
        line = line.strip().rstrip('\r')
        pieces = line.split(sep)
        if 'id_alumno' in pieces[col_id] or 'ID' in pieces[col_id] or 'MRUN' in pieces[col_id] or 'mrun' in pieces[col_id]:
            continue
        id_s = pieces[col_id]
        students[id_s] = {'pref':{}, 'bea': False}

        if yr == '2021':
            for i in range(10):
                idx1 = col_post + i # programs
                idx2 = col_post + i + 11 # scores
                students[id_s]['pref'][i+1] = {'cc':0, 'mc':0, 'pp':0, 'pa':0, 'od':0, 'prob':0}
                students[id_s]['pref'][i+1]['cc'] = int(pieces[idx1])
                try:
                    students[id_s]['pref'][i+1]['pp'] = float(pieces[idx2])
                except:
                    students[id_s]['pref'][i+1]['pp'] = -1

                if students[id_s]['pref'][i+1]['cc'] <= 0:
                    students[id_s]['pref'][i+1]['cc'] = 0

                # fix scores and marca
                if students[id_s]['pref'][i+1]['pp'] < 0: # i.e., equal to -1
                    students[id_s]['pref'][i+1]['pp'] = 0
                    students[id_s]['pref'][i+1]['mc'] = 0
                    students[id_s]['pref'][i+1]['od'] = 0
                    students[id_s]['pref'][i+1]['pa'] = 0
                else:
                    students[id_s]['pref'][i+1]['pp'] = int(round(students[id_s]['pref'][i+1]['pp']*100))
                    students[id_s]['pref'][i+1]['mc'] = 25
                    students[id_s]['pref'][i+1]['od'] = 0
                    students[id_s]['pref'][i+1]['pa'] = 0
                    if students[id_s]['pref'][i+1]['pp'] < 1000:
                        students[id_s]['pref'][i+1]['pp'] = students[id_s]['pref'][i+1]['pp']*100
                    elif students[id_s]['pref'][i+1]['pp'] < 10000:
                        students[id_s]['pref'][i+1]['pp'] = students[id_s]['pref'][i+1]['pp']*10
                    else:
                        pass
        else:
            for i in range(10):
                idx = col_post + 3*i # this is the column in the csv where the information of applications begins
                students[id_s]['pref'][i+1] = {'cc':0, 'mc':0, 'pp':0, 'pa':0, 'od':0, 'prob':0}
                students[id_s]['pref'][i+1]['cc'] = int(pieces[idx])
                students[id_s]['pref'][i+1]['mc'] = int(pieces[idx+1])
                students[id_s]['pref'][i+1]['pp'] = float(pieces[idx+2].replace(',', '.'))
                students[id_s]['pref'][i+1]['od'] = int(pieces[idx+3])
                students[id_s]['pref'][i+1]['pa'] = int(pieces[idx+4])

                # fix to have everyone with valid applications competing
                # if students[id_s]['pref'][i+1]['mc'] in [24, 25, 26]:
                #     students[id_s]['pref'][i+1]['mc'] = 25

                # fix scores so that all have five digits
                if students[id_s]['pref'][i+1]['pp'] < 1000:
                    students[id_s]['pref'][i+1]['pp'] = students[id_s]['pref'][i+1]['pp']*100
                elif students[id_s]['pref'][i+1]['pp'] < 10000:
                    students[id_s]['pref'][i+1]['pp'] = students[id_s]['pref'][i+1]['pp']*10
                else:
                    pass

        if info_bea:
            idx_bea = col_post + 5*10 # this is the column in the csv where the information of applications begins
            if pieces[idx_bea].isdigit():
                if int(pieces[idx_bea]) == 1:
                    students[id_s]['bea'] = True
            else:
                if pieces[idx_bea] == 'BEA':
                    students[id_s]['bea'] = True

    f.close()

    print 'Elapsed time:', time.time()-stime
    return students

def ReadBEA(filename):
    f = open(filename, 'r')
    bea = {}
    for line in f.readlines():
        line = line.strip().rstrip('\n')
        if 'mrun' in line:
            continue
        pieces = line.split(';')
        id = pieces[1]
        bea[id] = True
    return bea

def FindCutoffs(data):
    cutoffs = {}
    for id_s in data:
        for p in data[id_s]['pref']:
            if data[id_s]['pref'][p]['mc'] == 24:
                id_c = data[id_s]['pref'][p]['cc']
                if id_c not in cutoffs:
                    cutoffs[id_c] = 80000
                if data[id_s]['pref'][p]['pp'] < cutoffs[id_c]:
                    cutoffs[id_c] = data[id_s]['pref'][p]['pp']
    return cutoffs

def FindFirst(data):
    first = {}
    for id_s in data:
        for p in data[id_s]['pref']:
            if data[id_s]['pref'][p]['mc'] == 24:
                id_c = data[id_s]['pref'][p]['cc']
                if id_c not in first:
                    first[id_c] = 0
                if data[id_s]['pref'][p]['pp'] > first[id_c]:
                    first[id_c] = data[id_s]['pref'][p]['pp']
    return first

def FindAdmitted(data):
    admitted = {}
    for id_s in data:
        for p in data[id_s]['pref']:
            if data[id_s]['pref'][p]['mc'] == 24:
                id_c = data[id_s]['pref'][p]['cc']
                if id_c not in admitted:
                    admitted[id_c] = 0
                admitted[id_c]+=1
    return admitted

def StoreCutoffs():
    yr, inpath = '2020', '/Users/iriosu/Dropbox/Mistakes/Code/Python/Cartillas/'
    s19 = ReadApplications(inpath + os.sep + "data"+ os.sep + '2019' + os.sep + "C_POSTULANTES_SELECCION_PSU_2019_PUB_MRUN.csv", 1, 4, False)
    s20 = ReadApplications(inpath + os.sep + "data"+ os.sep + '2020' + os.sep + "C_POSTULANTES_SELECCION_PSU_2020_PUB_MRUN.csv", 1, 4, False)

    of19 = ReadPrograms(inpath + os.sep + "data"+ os.sep + '2019' + os.sep + 'carreras.csv')
    of20 = ReadPrograms(inpath + os.sep + "data"+ os.sep + '2020'+ os.sep + 'carreras.csv')
    of21 = ReadPrograms(inpath + os.sep + "data"+ os.sep + '2021'+ os.sep + 'carreras.csv')

    id_cs = sorted(list(set(of19.keys()).union(set(of20.keys()))))
    id_cs = sorted(list(set(id_cs).union(of21.keys())))

    c19 = FindCutoffs(s19)
    c20 = FindCutoffs(s20)

    f19 = FindFirst(s19)
    f20 = FindFirst(s20)

    ad19 = FindAdmitted(s19)
    ad20 = FindAdmitted(s20)

    out_c = {id_c:{'2019':{'min':0, 'max':0, 'full':0}, '2020':{'min':0, 'max':0, 'full':0}} for id_c in id_cs}
    for id_c in c19:
        out_c[id_c]['2019']['min'] = int(round(c19[id_c]))
        out_c[id_c]['2019']['max'] = int(round(f19[id_c]))
        out_c[id_c]['2019']['full'] = 1 if ad19[id_c] >= of19[id_c]['vacantes_reg'] else 0

    for id_c in c20:
        out_c[id_c]['2020']['min'] = int(round(c20[id_c]))
        out_c[id_c]['2020']['max'] = int(round(f20[id_c]))
        out_c[id_c]['2020']['full'] = 1 if ad20[id_c] >= of20[id_c]['vacantes_reg'] else 0

    with open(inpath + os.sep + 'outputs' + os.sep + 'cutoffs_2019_2020.pck', 'wb') as handle:
        pickle.dump(out_c, handle, protocol=pickle.HIGHEST_PROTOCOL)

    f = open(inpath + os.sep + 'outputs' + os.sep + 'first_last_full_2019_2020.csv', 'w')
    f.write("codigo_carrera;min_2019;max_2019;full_2019;min_2020;max_2020;full_2020\n")
    for id_c in sorted(out_c):
        f.write(str(id_c)+';'+str(out_c[id_c]['2019']['min'])+';'+str(out_c[id_c]['2019']['max'])+';'+str(out_c[id_c]['2019']['full'])+';'+str(out_c[id_c]['2020']['min'])+';'+str(out_c[id_c]['2020']['max'])+';'+str(out_c[id_c]['2020']['full'])+"\n")
    f.close()

def RandomizationTreatments(indir):
    def ReadInput(filename):
        out = {}
        f = open(filename, 'r')
        for line in f.readlines():
            line = line.strip().rstrip('\n')
            if 'nombre' in line:
                continue
            pieces = line.split(';')
            name = pieces[0]
            gender = pieces[1]
            id = pieces[2]
            out[id] = {'name':name, 'gender':gender}
        f.close()
        return out

    dat = ReadInput(indir + os.sep + 'Inscritos_final_febrero_MRUN.csv')

    for id_s in dat:
        dat[id_s]['rand'] = numpy.random.uniform(0,1)
        dat[id_s]['group'] = 0
        if dat[id_s]['rand'] < 1/3:
            dat[id_s]['group'] = 0
        elif dat[id_s]['rand'] >= 1/3 and dat[id_s]['rand'] < 2/3:
            dat[id_s]['group'] = 1
        else:
            dat[id_s]['group'] = 2

    f = open(indir + os.sep + 'treatment_assignment.csv', 'w')
    f.write("id;female;treatment_id\n")
    for id_s in dat:
        if 'M' in dat[id_s]['gender'] :
            f.write(str(id_s)+";FALSE;"+str(dat[id_s]['group'])+"\n")
        else:
            f.write(str(id_s)+";TRUE;"+str(dat[id_s]['group'])+"\n")
    f.close()

def ReadCutoffsCDF(filename):
    out = {}
    f = open(filename, 'r')
    for line in f.readlines():
        line = line.strip().rstrip('\n')
        if 'codigo' in line:
            continue
        pieces = line.split(';')
        id_c = int(pieces[0])
        out[id_c] = {(idx+199):float(pieces[idx]) for idx in range(1, len(pieces))}
    f.close()
    return out

def CheckDistributions():
    yr = '2021'
    num_cores = 1
    home_dir = os.path.expanduser("~")
    home_dir
    dropbox_dir = home_dir + os.sep + 'Dropbox/Mistakes/Code/Python/Cartillas'
    indir = dropbox_dir + os.sep + 'outputs' + os.sep + yr

    with open(dropbox_dir + os.sep + 'outputs' + os.sep + 'cutoffs_2019_2020.pck', 'rb') as handle:
        cutoffs_19_20 = pickle.load(handle)


    out_cdf = {'reg':{}, 'bea':{}}
    out_cdf['reg'] = ReadCutoffsCDF(indir + os.sep + 'cdf_cutoffs_reg.csv') #ReadCutoffsCDF(indir + os.sep + 'cdf_cutoffs_reg.csv')#/Users/iriosu/Dropbox/Mistakes/Code/Python/Cartillas/outputs/2021/day 2/cdf_cutoffs_reg.csv
    out_cdf['bea'] = ReadCutoffsCDF(indir + os.sep + 'cdf_cutoffs_bea.csv') #indir + os.sep + 'cdf_cutoffs_bea.csv'

    id_c = 33009
    cutoffs_19_20[id_c]
    bw_n, bw_p = -10, 10
    mn = min([pp for pp in out_cdf['reg'][id_c] if out_cdf['reg'][id_c][pp] > 0.01])
    mx = min([pp for pp in out_cdf['reg'][id_c] if out_cdf['reg'][id_c][pp] > 0.99])
    X = numpy.arange(mn+bw_n, mx+bw_p, 1)

    plt.figure()
    plt.plot(X, [out_cdf['reg'][id_c][int(pp)] for pp in X])
    plt.plot(X, [out_cdf['bea'][id_c][int(pp)] for pp in X])
    plt.show()


    for id_c in cutoffs_19_20:
        print id_c, cutoffs_19_20[id_c]




if __name__ == '__main__':
    indir = '/Users/iriosu/Dropbox/Mistakes/Code/Python/Cartillas/data/2021'
    apps = ReadApplications(indir + os.sep + 'Reporte_diario_postulaciones_final_mrun.csv', 0, 3, False, '2021', ',')

    bea = ReadBEA(indir + os.sep + '15-01-21_Preseleccionados_BEA_DEMRE_MRUN.csv')
    for id_s in apps:
        if id_s in bea:
            apps[id_s]['bea'] = True

    programs = ReadPrograms(indir + os.sep + 'carreras.csv',sep=';', yr='2021')

    indir
    # write in long format
    f = open(indir + os.sep + 'postulaciones_2021_final_long.csv', 'w')
    f.write('mrun;preferencia;codigo_carrera;puntaje_ponderado\n')
    for id_s in apps:
        for p in sorted(apps[id_s]['pref']):
            if apps[id_s]['pref'][p]['cc'] == 0:
                continue
            f.write(str(id_s) + ';' + str(p) + ';' + str(apps[id_s]['pref'][p]['cc']) + ';' + str(apps[id_s]['pref'][p]['pp']) + '\n')
    f.close()




# def ReadApplications(filename, col_id=0, col_post=3, info_bea=False):
#     stime = time.time()
#     f = open(filename, 'r')
#     lines = f.readlines()
#     students = {}
#     ct = 0
#     print('Storing applications of students')
#     for line in lines:
#         if ct%10000 == 0:
#             print( '    Students processed:', ct)
#         ct+=1
#         line = line.strip().rstrip('\r')
#         pieces = line.split(';')
#         if 'id_alumno' in pieces[col_id] or 'ID' in pieces[col_id] or 'MRUN' in pieces[col_id]:
#             continue
#         id_s = pieces[col_id]
#         students[id_s] = {'pref':{}, 'bea': False}
#         for i in range(10):
#             idx = col_post + 5*i # this is the column in the csv where the information of applications begins
#             students[id_s]['pref'][i+1] = {'cc':0, 'mc':0, 'pp':0, 'pa':0, 'od':0, 'prob':0}
#             students[id_s]['pref'][i+1]['cc'] = int(pieces[idx])
#             students[id_s]['pref'][i+1]['mc'] = int(pieces[idx+1])
#             students[id_s]['pref'][i+1]['pp'] = float(pieces[idx+2].replace(',', '.'))
#             students[id_s]['pref'][i+1]['od'] = int(pieces[idx+3])
#             students[id_s]['pref'][i+1]['pa'] = int(pieces[idx+4])
#
#             # fix to have everyone with valid applications competing
#             # if students[id_s]['pref'][i+1]['mc'] in [24, 25, 26]:
#             #     students[id_s]['pref'][i+1]['mc'] = 25
#
#             # fix scores so that all have five digits
#             if students[id_s]['pref'][i+1]['pp'] < 1000:
#                 students[id_s]['pref'][i+1]['pp'] = students[id_s]['pref'][i+1]['pp']*100
#             elif students[id_s]['pref'][i+1]['pp'] < 10000:
#                 students[id_s]['pref'][i+1]['pp'] = students[id_s]['pref'][i+1]['pp']*10
#             else:
#                 pass
#
#         if info_bea:
#             idx_bea = col_post + 5*10 # this is the column in the csv where the information of applications begins
#             if pieces[idx_bea].isdigit():
#                 if int(pieces[idx_bea]) == 1:
#                     students[id_s]['bea'] = True
#             else:
#                 if pieces[idx_bea] == 'BEA':
#                     students[id_s]['bea'] = True
#
#     f.close()
#
#     print('Elapsed time:', time.time()-stime)
#     return students
#
#
#
#
#
#     for id_s in dat:
#         dat[id_s]['rand'] = numpy.random.uniform(0,1)
#         dat[id_s]['group'] = 0
#         if dat[id_s]['rand'] < 1/3:
#             dat[id_s]['group'] = 0
#         elif dat[id_s]['rand'] >= 1/3 and dat[id_s]['rand'] < 2/3:
#             dat[id_s]['group'] = 1
#         else:
#             dat[id_s]['group'] = 2
#
#     f = open(indir + os.sep + 'treatment_assignment.csv', 'w')
#     f.write("id;female;treatment_id\n")
#     for id_s in dat:
#         if 'M' in dat[id_s]['gender'] :
#             f.write(str(id_s)+";FALSE;"+str(dat[id_s]['group'])+"\n")
#         else:
#             f.write(str(id_s)+";TRUE;"+str(dat[id_s]['group'])+"\n")
#     f.close()
#
#
#     yr, inpath = '2020', '/Users/iriosu/Dropbox/Mistakes/Code/Python/Cartillas/'
#
#     students = ReadApplications(inpath + os.sep + "data"+ os.sep + yr + os.sep + "C_POSTULANTES_SELECCION_PSU_"+yr+"_PUB_MRUN.csv", 1, 4, False)
#
#     dat = students
#
#     for id_s in dat:
#         dat[id_s]['rand'] = numpy.random.uniform(0,1)
#         dat[id_s]['group'] = 0
#         if dat[id_s]['rand'] < 1/3:
#             dat[id_s]['group'] = 0
#         elif dat[id_s]['rand'] >= 1/3 and dat[id_s]['rand'] < 2/3:
#             dat[id_s]['group'] = 1
#         else:
#             dat[id_s]['group'] = 2
#
#     f = open('/Users/iriosu/Dropbox/Mistakes/Code/Python/Cartillas/outputs/2020/treatment_assignment.csv', 'w')
#     f.write("id;treatment_id\n")
#     for id_s in dat:
#         f.write(str(id_s)+";"+str(dat[id_s]['group'])+"\n")
#     f.close()
