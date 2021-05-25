from __future__ import division
import sys, os, numpy, subprocess, multiprocessing, time, gc,math
import pickle
# import pandas as pd
import Core.Algoritmo, Core.Entidades

# -----------------------
# Main Methods
# -----------------------
def ReadScores(filename, sep=';'):
    stime = time.time()
    f = open(filename, 'r')
    lines = f.readlines()
    students = {}
    ct = 0
    print('Storing information of students')
    for line in lines:
        if ct%10000 == 0:
            print('    Students processed:', ct)
        ct+=1
        line = line.strip().rstrip('\r').replace('"','').replace(',','')
        if 'ID' in line or 'mrun' in line:
            continue
        pieces = line.split(sep)

        id_s = pieces[1]
        students[id_s] = Core.Entidades.Alumno(id_s)
        students[id_s].tipo_egreso = int(pieces[3])
        students[id_s].inscrito_proceso = 3  # consider by default both pools

        students[id_s].bea = False
        if 'BEA' in pieces[14]:
            students[id_s].bea = True

        students[id_s].nem = int(pieces[18])
        students[id_s].rank = int(pieces[19])

        students[id_s].lenguaje_actual = int(pieces[20])
        students[id_s].matematica_actual = int(pieces[21])
        students[id_s].historia_actual = int(pieces[22])
        students[id_s].ciencias_actual = int(pieces[23])
        students[id_s].promLM_actual = int(pieces[25])
        students[id_s].percLM_actual = int(pieces[25])

        students[id_s].lenguaje_anterior = int(pieces[27])
        students[id_s].matematica_anterior = int(pieces[28])
        students[id_s].historia_anterior = int(pieces[29])
        students[id_s].ciencias_anterior = int(pieces[30])
        students[id_s].promLM_anterior = int(pieces[32])
        students[id_s].percLM_anterior = int(pieces[33])
    f.close()
    print('Elapsed time:', time.time()-stime)
    return students

def ReadApplications(filename, col_id=0, col_post=3, info_bea=False, yr='2021', sep=';', scores=None):
    stime = time.time()
    f = open(filename, 'r')
    lines = f.readlines()
    students = {}
    ct = 0
    #print('Storing applications of students')
    for line in lines:
        #if ct%10000 == 0:
            #print('    Students processed:', ct)
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


    if scores is not None:
        for id_s in students:
            if id_s in scores:
                scores[id_s].pref = {p:students[id_s]['pref'][p]['cc'] for p in students[id_s]['pref']}
                scores[id_s].puntpond = {p:students[id_s]['pref'][p]['pp'] for p in students[id_s]['pref']}
                scores[id_s].marca = {p:students[id_s]['pref'][p]['mc'] for p in students[id_s]['pref']}
                scores[id_s].puntano = {p:students[id_s]['pref'][p]['pa'] for p in students[id_s]['pref']}


    #print('Elapsed time:', time.time()-stime)
    return students

def ReadPrograms(filename,sep=';'):
    f = open(filename, 'r')
    lines = f.readlines()
    programs = {}
    ct = 0
    print('Storing information of programs')
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
        programs[id_c].max_post = int(pieces[21])-1 if pieces[21].isdigit() else 0

        programs[id_c].vacantes_bea = int(pieces[22]) if pieces[22].isdigit() else 0

        programs[id_c].excluye_egresado_antes = int(pieces[23]) if pieces[23].isdigit() else 0

        if 'S' in pieces[24]:
            programs[id_c].pedagogia = True

        programs[id_c].min_ranking = int(pieces[25]) if pieces[25].isdigit() else 0

    f.close()
    return programs

def RunPreprocessing(alumno, carrera):
    '''
    Receives as input a student object and a program object and returns weighted score and mark
    '''
    puntajes = {'nem': int(alumno.nem), 'rank': int(alumno.rank), 'actual':{}, 'anterior':{}}
    puntajes['actual']['lenguaje'],puntajes['actual']['matematica'], puntajes['actual']['historia'], puntajes['actual']['ciencias'] = int(alumno.lenguaje_actual), int(alumno.matematica_actual), int(alumno.historia_actual), int(alumno.ciencias_actual)
    puntajes['anterior']['lenguaje'],puntajes['anterior']['matematica'], puntajes['anterior']['historia'], puntajes['anterior']['ciencias'] = int(alumno.lenguaje_anterior), int(alumno.matematica_anterior), int(alumno.historia_anterior), int(alumno.ciencias_anterior)

    if alumno.inscrito_proceso == 1:
    	pool = {'actual': [True, 0, 0]} # primer elemento dice si pool es valido, el segundo guarda la marca y el tercero el puntaje ponderado
    elif alumno.inscrito_proceso == 2:
    	pool = {'anterior': [True, 0, 0]} # primer elemento dice si pool es valido, el segundo guarda la marca y el tercero el puntaje ponderado
    elif alumno.inscrito_proceso == 3:
    	pool = {'actual': [True, 0, 0], 'anterior': [True, 0, 0]} # primer elemento dice si pool es valido, el segundo guarda la marca y el tercero el puntaje ponderado
    else:
    	sys.stdout.write('\n***ERROR: el alumno ' + str(a) + ' tiene un valor para inscrito_proceso igual a ' + str(alumno.inscrito_proceso) + ', lo que es invalido.')
    	sys.exit('Error al definir data para escribir en archivos de salida; inscrito proceso desconocido; metodo Preprocesamiento, Asignacion.py')


    puntpond, marca, puntano = 0,0,0
	# Diccionario para almacenar datos relativos al procesamiento de esta preferencia

    pool = {'actual': [True, 0, 0], 'anterior': [True, 0, 0]} # primer elemento dice si pool es valido, el segundo guarda la marca y el tercero el puntaje ponderado

    ponderaciones = {'nem': carrera.nem, 'rank': carrera.rank, 'lenguaje': carrera.lenguaje, 'matematica': carrera.matematica, 'historia': carrera.historia, 'ciencias': carrera.ciencias}

	# Procesamiento de la preferencia, ante la primera causal de eliminacion se sale del while.
    for proceso in pool: # revisamos cada pool
    	# Preferencia invalida
    	if puntajes[proceso]['lenguaje'] == 0: # no tiene puntaje de lenguaje
    		pool[proceso] = [False, 27, 0]
    		continue
    	if puntajes[proceso]['matematica'] == 0: # no tiene puntaje de matematica
    		pool[proceso] = [False, 28, 0]
    		continue
    	if (alumno.nem == 0 or alumno.nem is None) and alumno.tipo_egreso not in ['4','8']:# Falta NEM
    		pool[proceso] = [False, 9, 0]
    		continue
    	if int(100*0.5*(puntajes[proceso]['lenguaje'] + puntajes[proceso]['matematica'])) < 100*450: # no cumple promedio minimo establecido por cruch
    		pool[proceso] = [False, 36, 0]
    		continue
    	cu,pp,ptj_pp,escala = int(carrera.codigo_carrera/1000),0,0,1
    	if puntajes[proceso]['historia'] == 0 and puntajes[proceso]['ciencias'] == 0: #no rinde ninguna de las pruebas electivas
    		pool[proceso] = [False, 35, 0]
    		continue
    	if not carrera.prueba_electiva: # el alumno puede escoger entre la prueba de ciencias y la prueba de historia
    		if puntajes[proceso]['historia'] == 0 and ponderaciones['historia'] > 0: #exige prueba historia y alumno no la rinde
    			pool[proceso] = [False, 29, 0]
    			continue
    		elif puntajes[proceso]['ciencias'] == 0 and ponderaciones['ciencias'] > 0: #exige prueba ciencias y alumno no la rinde
    			pool[proceso] = [False, 30, 0]
    			continue
        setattr(alumno, 'promLM_'+proceso, int(100*0.5*(puntajes[proceso]['lenguaje'] + puntajes[proceso]['matematica'])))
        if int(100*0.5*(puntajes[proceso]['lenguaje'] + puntajes[proceso]['matematica'])) < 100*carrera.promedio_minimo: # no cumple promedio minimo
        	pool[proceso] = [False, 31, 0]
        	continue

        if alumno.tipo_egreso in ['4', '8']: #alumno con estudios en extranjero - no tiene nem ni ranking
        	escala = (100 - (ponderaciones['nem'] + ponderaciones['rank']))/100
        	pp = (sum([puntajes[proceso][factor]*ponderaciones[factor] for factor in ['lenguaje', 'matematica']]) \
        		  + max([puntajes[proceso][factor]*ponderaciones[factor] for factor in ['historia', 'ciencias']]))/escala
        	ptj_pp = pp/100 # puntaje para registros
        	pp = int(round(pp)) # puntaje con el que el alumno compite
        else:
        	pp = (sum([puntajes[proceso][factor]*ponderaciones[factor] for factor in ['lenguaje', 'matematica']]) \
        		  + max([puntajes[proceso][factor]*ponderaciones[factor] for factor in ['historia', 'ciencias']]))
        	pp += sum([puntajes[factor]*ponderaciones[factor] for factor in ['nem', 'rank']])
        	ptj_pp = pp/100 # puntaje para registros
        	pp = int(round(pp)) # puntaje con el que el alumno compite

        if pp < carrera.ponderado_minimo: # si no cumple puntaje ponderado minimo de la carrera
        	pool[proceso] = [False, 17, 0] #pp
        	continue
        else:
        	pool[proceso] = [True, 25, pp]

    if alumno.inscrito_proceso == 1:
    	marca = pool['actual'][1]
    	puntpond = pool['actual'][2]
    	puntano = 1
    elif alumno.inscrito_proceso == 2:
    	marca = pool['anterior'][1]
    	puntpond = pool['anterior'][2]
    	puntano = 2
    elif alumno.inscrito_proceso == 3:
    	if not (pool['anterior'][0] or pool['actual'][0]): # si ninguno de los pools es valido, consideramos la marca del ultimo pool
    		marca = pool['actual'][1]
    		puntpond = pool['actual'][2]
    		puntano = 1
    	elif not pool['anterior'][0] and pool['actual'][0]: # pool del ano de egreso valido y anterior invalido
    		marca = pool['actual'][1]
    		puntpond = pool['actual'][2]
    		puntano = 1
    	elif pool['anterior'][0] and not pool['actual'][0]: # pool del ano anterior valido y de egreso invalido
    		marca = pool['anterior'][1]
    		puntpond = pool['anterior'][2]
    		puntano = 2
    	else: #ambos pools validos
    		mx = 0
    		for proceso in pool:
    			if pool[proceso][2] >= mx:
    				marca = pool[proceso][1]
    				puntpond = pool[proceso][2]
    				if proceso == 'actual':
    					puntano = 1
    				else:
    					puntano = 2
    				mx = pool[proceso][2]
    else:
    	sys.stdout.write('\n***ERROR: el alumno ' + str(a) + ' tiene un valor para inscrito_proceso igual a ' + str(alumno.inscrito_proceso) + ', lo que es invalido.')
    	sys.exit('Error al definir data para escribir en archivos de salida; inscrito_proceso desconocido; metodo Preprocesamiento, Asignacion.py')

    return int(math.floor(puntpond/100))

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

def SubRoutine(indata):
    id_s,students, programs, out_cdf, infile = indata[0], indata[1], indata[2], indata[3], indata[4]
    out = {}
    for id_c in sorted(programs):
        pp = RunPreprocessing(students[id_s], programs[id_c])
        if pp <= 200:
            out[id_c] = 0
        else:
            if students[id_s].bea:
                out[id_c] = out_cdf['bea'][id_c][pp]
            else:
                try:
                    out[id_c] = out_cdf['reg'][id_c][pp]
                except:
                    print out.cdf.keys()
                    print id_c
                    sys.exit(1)
    f = open(infile, 'a')
    f.write(str(id_s) + ';' + ';'.join([str(out[id_c]) for id_c in sorted(out)]) + '\n')
    f.close()


if __name__ == '__main__':
    yr = '2021'
    num_cores = 1
    home_dir = os.path.expanduser("~")
    home_dir
    dropbox_dir = home_dir + os.sep + 'Dropbox/Mistakes/Code/Python/Cartillas'
    indir = dropbox_dir + os.sep + 'outputs' + os.sep + yr

    # 1. Read programs and vacancies
    programs = ReadPrograms(dropbox_dir + os.sep + 'data/'+yr+'/carreras.csv')

    # 2. Read scores
    # scores = ReadScores('input/C_2019.csv')
    students = ReadScores(dropbox_dir + os.sep + 'data/'+yr+'/adm2021_archivo_C_demre_2021feb08_MRUN_Compartir.csv', ';')

    # 3. Read applications
    applications = ReadApplications(dropbox_dir + os.sep +  "data" + os.sep + yr + os.sep + 'Reporte_diario_postulaciones_2_mrun.csv', 0, 3, False, '2021', ',', students)

    # 4. Perform preprocessing
    # with open(indir + os.sep + 'cdf_cutoffs.pck', 'rb') as handle:
    #     out_cdf = pickle.load(handle)

    out_cdf = {'reg':{}, 'bea':{}}
    out_cdf['reg'] = ReadCutoffsCDF(indir + os.sep + 'cdf_cutoffs_reg.csv')
    out_cdf['bea'] = ReadCutoffsCDF(indir + os.sep + 'cdf_cutoffs_bea.csv')

    # out = {}
    # ct = 0
    # print('Preprocessing')
    # for id_s in applications:
    #     if id_s not in students:
    #         continue
    #     if ct%1000 == 0:
    #         print('    Students processed:', ct)
    #     ct+=1
    #     out[id_s] = {}
    #     for id_c in sorted(programs):
    #         # print(id_s, id_c)
    #         pp = RunPreprocessing(students[id_s], programs[id_c])
    #         if pp <= 200:
    #             out[id_s][id_c] = 0
    #         else:
    #             out[id_s][id_c] = out_cdf[id_c][pp]
    #     if ct > 1000:
    #         break
    #
    # # 5. Store outcome of preprocessing
    # f = open(indir + os.sep + 'prob_in_each_program.csv', 'w')
    # f.write('mrun;' + ';'.join([str(id_c) for id_c in sorted(programs)]) + '\n')
    # for id_s in out:
    #     f.write(str(id_s) + ';' + ';'.join([str(out[id_s][id_c]) for id_c in sorted(out[id_s])]) + '\n')
    # f.close()


    f = open(indir + os.sep + 'prob_in_each_program.csv', 'w')
    f.write('mrun;' + ';'.join([str(id_c) for id_c in sorted(programs)]) + '\n')
    f.close()

    indata = []
    ct = 0
    for id_s in applications:
        if id_s not in students:
            continue
        if num_cores == 1:
            SubRoutine((id_s,students, programs,out_cdf, indir + os.sep + 'prob_in_each_program.csv'))
        else:
            indata.append((id_s,students, programs,out_cdf, indir + os.sep + 'prob_in_each_program.csv'))
        ct+=1
        if ct > 1000:
            break

    if num_cores > 1:
        np = min(num_cores,multiprocessing.cpu_count())
        pool = multiprocessing.Pool(processes=min(np, len(indata)))
        pool.map(SubRoutine, indata)
        pool.close()
        pool.join()






    # with open(indir + os.sep + 'cdf_cutoffs.pck', 'rb') as handle:
    #     out_cdf = pickle.load(handle)
    #
    # f = open(indir + os.sep + 'cdf_cutoffs.csv', 'w')
    # f.write('codigo_carrera;'+';'.join(str(pp) for pp in numpy.arange(200, 850, 1))+"\n")
    # for id_c in sorted(out_cdf):
    #     f.write(str(id_c)+ ';' +';'.join(str(out_cdf[id_c][pp]) for pp in numpy.arange(200, 850, 1))+"\n")
    # f.close()
