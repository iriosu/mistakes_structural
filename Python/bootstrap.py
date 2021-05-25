from __future__ import division
import sys, os, numpy, subprocess, multiprocessing, time, gc, glob
import copy
import pickle
import Core.Algoritmo, Core.Entidades


numpy.random.seed(20181229)

# we start with reading the data. We use the files provided by DEMRE (from 2004 to 2017)
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
        students[id_s] = {'pref':{}, 'bea': False, 'lml': None}

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

def ReadPrograms(filename,sep=';'):
    f = open(filename, 'r')
    lines = f.readlines()
    programs = {}
    ct = 0
    print 'Storing information of programs'
    for line in lines:
        line = line.strip().rstrip('\r')
        pieces = line.split(sep)
        if 'CODIGO' in pieces[0]:
            continue
        id_c = int(pieces[0])
        programs[id_c] = Core.Entidades.Carrera(id_c)
        programs[id_c].nem = int(pieces[3]) if pieces[3].isdigit() else 0
        programs[id_c].rank = int(pieces[4]) if pieces[4].isdigit() else 0
        programs[id_c].lenguaje = int(pieces[5]) if pieces[5].isdigit() else 0
        programs[id_c].matematica = int(pieces[6]) if pieces[6].isdigit() else 0
        programs[id_c].historia = int(pieces[7]) if pieces[7].isdigit() else 0
        programs[id_c].ciencias = int(pieces[8]) if pieces[8].isdigit() else 0
        programs[id_c].promedio_minimo = int(pieces[9]) if pieces[9].isdigit() else 0
        programs[id_c].ponderado_minimo = int(pieces[10])*100 if pieces[10].isdigit() else 0
        programs[id_c].ponderado_minimo_antes_pe_pond = int(pieces[11])*100 if pieces[11].isdigit() else 0
        programs[id_c].vacantes_reg = 0
        for r in range(12, 16):
            seats = int(pieces[r]) if pieces[r].isdigit() else 0
            programs[id_c].vacantes_reg = programs[id_c].vacantes_reg + seats

        if 'S' in pieces[16]:
			programs[id_c].restringe_sexo = True

        if 'S' in pieces[17]:
            programs[id_c].prueba_especial = True
            programs[id_c].tipo_prueba_especial = pieces[18]
            programs[id_c].pct_prueba_especial = int(pieces[19]) if pieces[19].isdigit() else 0

        if 'S' in pieces[20]:
			programs[id_c].prueba_electiva = True


        programs[id_c].excluye_desde_pref = int(pieces[21]) if pieces[21].isdigit() else 0
        programs[id_c].max_post = int(pieces[21]) if pieces[21].isdigit() else 0

        programs[id_c].vacantes_bea = int(pieces[22]) if pieces[22].isdigit() else 0

        programs[id_c].excluye_egresado_antes = int(pieces[23]) if pieces[23].isdigit() else 0

        if 'S' in pieces[24]:
            programs[id_c].pedagogia = True

        programs[id_c].min_ranking = int(pieces[25]) if pieces[25].isdigit() else 0

    f.close()
    return programs

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

def ReadLML(filename, sep=';'):
    f = open(filename, 'r')
    lml = {}
    for line in f.readlines():
        line = line.strip().rstrip('\n')
        if 'mrun' in line:
            continue
        pieces = line.split(sep)
        id = pieces[0]
        level = pieces[1]
        lml[id] = level
    return lml

def BootstrapIteration(data):
    dat_pg, dat_st, boot_list, s, outdir, unified, alg, boo_stb = data[0], data[1], data[2], data[3], data[4], data[5], data[6], data[7]
    carreras = copy.deepcopy(dat_pg)
    alumnos = {}
    ct = 0
    for i in range(len(boot_list)):
        alumnos[ct] = Core.Entidades.Alumno(ct)
        dat_aux = dat_st[boot_list[i]]['pref']
        alumnos[ct].pref = {j: dat_aux[j]['cc'] for j in dat_aux if dat_aux[j]['cc'] > 0}
        alumnos[ct].puntpond = {j: dat_aux[j]['pp'] for j in dat_aux if dat_aux[j]['cc'] > 0}
        alumnos[ct].marca = {j: dat_aux[j]['mc'] for j in dat_aux if dat_aux[j]['cc'] > 0}
        alumnos[ct].puntano = {j: dat_aux[j]['pa'] for j in dat_aux if dat_aux[j]['cc'] > 0}
        alumnos[ct].bea = dat_st[boot_list[i]]['bea']
        ct+=1

    if unified == True:
        Core.Algoritmo.EjecutarSeleccion(alumnos, carreras, ['reg','bea'], alg, outdir, s, boo_stb)
    else:
        Core.Algoritmo.EjecutarSeleccion(alumnos, carreras, ['reg'], alg, outdir, s, boo_stb)
        Core.Algoritmo.EjecutarSeleccion(alumnos, carreras, ['bea'], alg, outdir, s, boo_stb)

    # store outputs
    if unified == True:
        f = open(os.path.join(outdir, 'cutoffs_and_extras_unica_'+alg+'_s='+str(s)+'.csv'), 'w')
    else:
        f = open(os.path.join(outdir, 'cutoffs_and_extras_secuencial_'+alg+'_s='+str(s)+'.csv'), 'w')

    f.write("codigo_carrera;cutoff_reg;cutoff_bea;seleccionados_reg;seleccionados_bea;vacantes_reg;vacantes_bea;extras_reg;extras_bea\n")
    for c in sorted(carreras):
        f.write(str(c)+';'+str(carreras[c].cutoff['reg'])+';'+str(carreras[c].cutoff['bea'])+';'+str(carreras[c].seleccionados['reg'])+';'+str(carreras[c].seleccionados['bea'])+';'+str(carreras[c].vacantes_reg)+';'+str(carreras[c].vacantes_bea)+';'+str(carreras[c].vacantes_extra['reg'])+';'+str(carreras[c].vacantes_extra['bea'])+'\n')
    f.close()

def ConsolidateSimulations(indir):
    cutoffs = {}
    for filename in os.listdir(indir):
        if 'cutoffs' not in filename:
            continue
        if 'own' in filename:
            continue
        if 'pck' in filename:
            continue
        # open file and read content
        f = open(os.path.join(indir, filename), 'r')
        for line in f.readlines():
            line = line.strip().rstrip('\n')
            if 'codigo_carrera' in line:
                continue
            pieces = line.split(';')
            id_c = int(pieces[0])
            if id_c not in cutoffs:
                cutoffs[id_c] = {'reg':[], 'bea':[]}
            cutoffs[id_c]['reg'].append(int(round(float(pieces[1]))))
            cutoffs[id_c]['bea'].append(int(round(float(pieces[2]))))
        f.close()

    g = open(os.path.join(indir, 'consolidated_reg.csv'), 'w')
    program_codes = sorted(cutoffs.keys())
    st = ';'.join(map(str,program_codes))
    g.write(st + '\n')
    for i in range(len(cutoffs[program_codes[0]]['reg'])):
        aux = [cutoffs[c]['reg'][i] for c in program_codes]
        st = ';'.join(map(str, aux))
        g.write(st + '\n')
    g.close()

    g = open(os.path.join(indir, 'consolidated_bea.csv'), 'w')
    st = ';'.join(map(str,program_codes))
    g.write(st + '\n')
    for i in range(len(cutoffs[program_codes[0]]['bea'])):
        aux = [cutoffs[c]['bea'][i] for c in program_codes]
        st = ';'.join(map(str, aux))
        g.write(st + '\n')
    g.close()

def ReadCutoffs(filename, cutoffs):
    f = open(filename, 'r')
    output = {}
    for line in f.readlines():
        if 'codigo_carrera' in line:
            continue
        line = line.strip().rstrip('\n')
        pieces = line.split(';')
        cc = int(pieces[0])
        reg = float(pieces[1])/100
        bea = float(pieces[2])/100
        if cc not in output:
            output[cc] = {}
        output[cc]['reg'] = reg
        output[cc]['bea'] = bea
    f.close()
    return output

def ReadSimulations(indir, num_sims):
    cutoffs = {}
    for s in range(num_sims):
        aux = ReadCutoffs(indir + os.sep + 'cutoffs_and_extras_unica_student_s='+str(s)+".csv", cutoffs)
        for id_c in aux:
            if id_c not in cutoffs:
                cutoffs[id_c] = {'reg':[], 'bea':[]}
            cutoffs[id_c]['reg'].append(aux[id_c]['reg'])
            cutoffs[id_c]['bea'].append(aux[id_c]['bea'])
    return cutoffs

if __name__ == '__main__':
    '''
    Process to perform bootstrap from partial applications. The file has to:
    1. Read program characteristics.
    2. Read partial applications.
    3. Bootstrap new applications to complete instance.
    4. Run assignment algorithm
    5. Repeat 2 to 4 S times to obtain distribution of cutoffs for both REG and BEA processes.
    '''


    #### real ####
    num_cores = 1
    num_sims = 1 # bootstrap iterations
    yr = '2021'

    boo_stb = False # not used here.
    algorithm = 'student'
    unified = True

    inpath = '/Users/iriosu/Dropbox/Mistakes/Code/Python/Cartillas/'
    outdir = inpath + os.sep + 'outputs' + os.sep + yr
    if not os.path.exists(outdir):
        os.makedirs(outdir)

    # 1. read programs and vacancies
    programs = ReadPrograms(inpath + os.sep + "data"+ os.sep + yr + os.sep + 'carreras.csv')

    # id_c = 55112
    # print programs[id_c].codigo_carrera
    # print programs[id_c].vacantes_reg
    # print programs[id_c].vacantes_bea
    # print programs[id_c].nem
    # print programs[id_c].rank
    # print programs[id_c].lenguaje
    # print programs[id_c].ciencias
    # print programs[id_c].historia
    # print programs[id_c].prueba_electiva
    # print programs[id_c].restringe_sexo
    # print programs[id_c].tipo_prueba_especial
    # print programs[id_c].pct_prueba_especial
    # print programs[id_c].promedio_minimo
    # print programs[id_c].ponderado_minimo
    # print programs[id_c].ponderado_minimo_antes_pe_pond
    # print programs[id_c].max_post
    # print programs[id_c].excluye_desde_pref
    # print programs[id_c].excluye_egresado_antes
    # print programs[id_c].pedagogia
    # sys.exit(1)

    # 2. read applications
    students = ReadApplications(inpath + os.sep + "data"+ os.sep + yr + os.sep + 'Reporte_diario_postulaciones_final_mrun.csv', 0, 3, False, '2021', ',')
    lml = ReadLML(inpath + os.sep + "data"+ os.sep + yr + os.sep + 'plm_level.csv', ',')
    bea = ReadBEA(inpath + os.sep + "data"+ os.sep + yr + os.sep + '15-01-21_Preseleccionados_BEA_DEMRE_MRUN.csv')
    for id_s in students:
        if id_s in bea:
            students[id_s]['bea'] = True
        if id_s in lml:
            students[id_s]['lml'] = lml[id_s]

    # 3. Sample students with replacement for bootstrap
    student_ids = students.keys()
    prs = ['low', 'mid', 'high']
    ids = {pr:[] for pr in prs}
    for id_s in students:
        if 'lml' not in students[id_s]:
            continue
        if students[id_s]['lml'] is None:
            continue
        for pr in prs:
            if pr in students[id_s]['lml']:
                ids[pr].append(id_s)
                break

    # 4. Define target market
    targets = {'low':1229, 'mid':3620, 'high':18402}
    boot_samp = {pr: numpy.random.choice(ids[pr], (num_sims, targets[pr]), True) for pr in ids}

    # Merge existing students with sampled ones
    boostrap_market = numpy.tile(student_ids, (boot_samp['low'].shape[0],1))
    for pr in boot_samp:
        boostrap_market = numpy.column_stack([boot_samp[pr], boostrap_market])

    # 5. Run Bootstrap
    # s>0 considers random tie-breaker
    indata = []
    for sim in range(num_sims):
        indata.append((programs, students, boostrap_market[sim], sim, outdir, unified, algorithm, boo_stb))
        if num_cores == 1:
            BootstrapIteration((programs, students, boostrap_market[sim], sim, outdir, unified, algorithm, boo_stb))

    if num_cores > 1:
        np = min(num_cores,multiprocessing.cpu_count())
        pool = multiprocessing.Pool(processes=min(np, len(indata)))
        pool.map(BootstrapIteration, indata)
        pool.close()
        pool.join()


    cutoffs = ReadSimulations(outdir, num_sims)

    ConsolidateSimulations(outdir)

    with open(outdir + os.sep + 'cutoffs.pck', 'wb') as handle:
        pickle.dump(cutoffs, handle, protocol=pickle.HIGHEST_PROTOCOL)

    for filename in glob.glob(outdir+ os.sep + 'cutoffs_and_extras_unica_student_s=*'):
        os.remove(filename)
