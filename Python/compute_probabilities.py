from __future__ import division
import sys, os, numpy as np, subprocess, multiprocessing, time, gc
import pickle
import scipy.stats
from sklearn.neighbors import KernelDensity
import functools
# import matplotlib.pyplot as plt
from scipy.stats import truncnorm
import pandas as pd
import math

def tidy_up_bootstraps(df, programs):
    """
    tidies up bootstraps. this mean dealing with cutoff realizations that
    are zero.

    Note: replace 0 cutoffs.
    If all cutoffs from simulations are zero put 210
    (this is min weighted score satisfying mat-len>450)
    Otherwise, replace 0's with minimum cutoffs across simulations
    that is bigger than 0

    if the row has zeroes, replace with row min (non zero)
    if all cutoffs are 0, replace with 210

    Arguments:
        df: cutoff bootstrap files made by ignacio

    Returns:
        transposed bootstraps with non-zero values.
    """
    # Replace zero cutoffs with minimo ponderado
    for (id_c, columnData) in df.iteritems():
        #id_c = 11001
        #print('Colunm Name : ', id_c)
        #print('Column Contents : ', columnData.values)
        df[id_c] = np.maximum(df[id_c], max(programs[int(id_c)]['ponderado_minimo'], 21000))

    # get values
    bs = df.values
    # transpose cutoffs
    bs = bs.T
    # bring down to scale
    bs = bs / 100

    # for majors where all bs replicas are 0, replace with 210 (around 4%)
    #bs[bs.mean(axis=1) == 0] = 210

    # replace all zeros with row minimums
    bs = np.where(bs == 0,
                  np.nanmin(np.where(bs != 0, bs, np.nan).T, axis=1),
                  bs)
    assert bs.min() != 0

    return bs

def get_mle_params(bs, df, programs):
    """
    Estimates parameters of a truncated normal via MLE for majorid.

    Arguments:
        bs: bootstrap replicates of cutoffs already tidied up

    Returns
        alpha, beta, mean and variance of the distribution of cutoffs
    """
    # fetch lower and upper trunc normal values.
    # Changing a to be the max between minimum weighted score and 210, and
    # b to be 850
    lowers = []
    uppers = []

    for (id_c, columnData) in df.iteritems():
        #print('Colunm Name : ', id_c)
        #print('Column Contents : ', columnData.values)
        lowers.append(max(programs[int(id_c)]['ponderado_minimo']/100,210.0))
        uppers.append(850.0)

    #lowers = bs.min(axis=1)
    #uppers = bs.max(axis=1)

    # get mu and sigma
    locs = bs.mean(axis=1)
    scales = bs.std(axis=1)
    # impute majors with 0 variance with lowest non-zero variance
    scales_min = np.nanmin(np.where(scales == 0, np.nan, scales))

    # standardize lower and upper since that's how scipy likes it
    alphas = (lowers - locs) / np.maximum(scales, scales_min)
    betas = (uppers - locs) / np.maximum(scales, scales_min)

    #Replace zero alphas with predetermined value
    alphas = np.minimum(alphas,-0.01)
    #alphas[3]
    #scales[3]

    mle_params = []
    for i, replicates in enumerate(bs):
        # get MLE estimates holding a and b fixed
        if scales[i] > 0.0:
            est_params = truncnorm.fit(replicates,
                                       fa=alphas[i],
                                       fb=betas[i],
                                       loc=locs[i],
                                       scale=scales[i])

            # make sure that negative log likelihood from MLE is NOT higher than
            # using the parameters from the sample
            guess_params = (alphas[i], betas[i], locs[i], scales[i])
            nnlfa = truncnorm.nnlf(guess_params, replicates)
            nnlfb = truncnorm.nnlf(est_params, replicates)

            np.testing.assert_almost_equal(actual=nnlfa,
                desired=nnlfb,
                err_msg='neg-log-likelihoods are bad')
        else:
            est_params = (alphas[i], betas[i], locs[i], scales[i])

        mle_params.append(est_params)


    return mle_params

def estimate_cutoffs_params(indir, programs, proceso='reg'):
    """
    estimates parameters for cutoff distributions for a specific year using
    parameters for a truncated normal distribution.

    Arguments:
        year: process year

    Returns:
        table with parameters as columns and majors as rows

    """
    # load bootstrap cutoffs
    df = pd.read_csv(indir + os.sep + 'consolidated_'+proceso+'.csv', delimiter=';')
    bs = tidy_up_bootstraps(df, programs)

    params = get_mle_params(bs, df, programs)
    pardf = pd.DataFrame(params).set_index(df.columns)

    # # tidy up
    pardf.reset_index(inplace=True)
    pardf.columns = ['majorid', 'a', 'b', 'mu', 'sigma']

    return pardf

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
    #print 'Storing applications of students'
    for line in lines:
        #if ct%10000 == 0:
            #print '    Students processed:', ct
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

    #print 'Elapsed time:', time.time()-stime
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


if __name__ == '__main__':
    #indir = '/Users/iriosu/Dropbox/Mistakes/Code/Python/Cartillas/outputs/2020/'
    yr = '2021'
    home_dir = os.path.expanduser("~")
    home_dir
    dropbox_dir = home_dir + os.sep + 'Dropbox/Mistakes/Code/Python/Cartillas'
    outdir = dropbox_dir + os.sep + 'outputs' + os.sep + yr

    # -----------------------------------
    # Load Data
    # -----------------------------------
    # 1. Read simulated cutoffs
    with open(outdir + os.sep + 'cutoffs.pck', 'rb') as handle:
        cutoffs = pickle.load(handle)

    programs = ReadPrograms(dropbox_dir + os.sep + "data"+ os.sep + yr + os.sep + 'carreras.csv',sep=';', yr='2021')

    # -----------------------------------
    # Estimate CDF
    # -----------------------------------
    # 2b For each program, estimate Truncated Normal Kernel via MLE and cdf
    d = {}
    d['reg'] = estimate_cutoffs_params(outdir, programs, 'reg')
    d['bea'] = estimate_cutoffs_params(outdir, programs, 'bea')

    #d[d['majorid'] == str(50112)]
    d['reg'].to_csv(outdir + os.sep + 'mle_params_reg_'+yr+'.csv', index=False)
    d['bea'].to_csv(outdir + os.sep + 'mle_params_bea_'+yr+'.csv', index=False)

    domain = np.arange(200, 850, 1)
    out_cdf = {id_c:{'reg':{}, 'bea':{}} for id_c in cutoffs}
    for pr in ['reg', 'bea']:
        for id_c in sorted(cutoffs):
            # 2.1 Read simulated cutoffs
            x = np.array(cutoffs[id_c][pr]) #np.random.normal(72000, 1000, 10000)#np.array(cutoffs[11045]['reg'])
            x_grid = np.linspace(min(x)-30, max(x)+30, 50)

            # 2.2 Estimate kernel and plot distribution
            d_car = d[pr][d[pr]['majorid'] == str(id_c)]
            a     = d_car['a']
            b     = d_car['b']
            mu    = d_car['mu']
            sigma = d_car['sigma']

            # Replace zeros in a , b and sigmas
            if a.item() > -0.01:
                a = -0.01
            if b.item() < 0.01:
                b = 0.01
            if sigma.item() < 0.01:
                sigma = 0.01

            # Compute pdf and pdf
            pdf = truncnorm.pdf(x_grid,a,b,mu,sigma)
            cdf = truncnorm.cdf(x_grid,a,b,mu,sigma)

            # Plot distribution
            # plt.figure()
            # plt.plot(x_grid, pdf, color='blue', alpha=0.5, lw=3)
            # plt.hist(x,10,density = 1,color ='green',alpha = 0.7)
            # plt.savefig(outdir + os.sep + 'plots' + os.sep + 'dist_'+str(id_c)+'.pdf')

            # 2.3 Compute CDF and store
            cum_dist = truncnorm.cdf(domain,a,b,mu,sigma)
            for key, val in zip(domain, cum_dist):
                out_cdf[id_c][pr][key] = val

    # 3. Store CDF in pickle file.
    with open(outdir + os.sep +'cdf_cutoffs.pck', 'wb') as handle:
        pickle.dump(out_cdf, handle, protocol=pickle.HIGHEST_PROTOCOL)

    f = open(outdir + os.sep + 'cdf_cutoffs_reg.csv', 'w')
    f.write('codigo_carrera;'+';'.join(str(pp) for pp in np.arange(200, 850, 1))+"\n")
    for id_c in sorted(out_cdf):
        f.write(str(id_c) + ';' +';'.join(str(out_cdf[id_c]['reg'][pp]) for pp in np.arange(200, 850, 1))+"\n")
    f.close()

    f = open(outdir + os.sep + 'cdf_cutoffs_bea.csv', 'w')
    f.write('codigo_carrera;'+';'.join(str(pp) for pp in np.arange(200, 850, 1))+"\n")
    for id_c in sorted(out_cdf):
        f.write(str(id_c) + ';' +';'.join(str(out_cdf[id_c]['bea'][pp]) for pp in np.arange(200, 850, 1))+"\n")
    f.close()

    # -----------------------------------
    # Write probabilities in applicants file
    # -----------------------------------
    # # 1. Read simulated cutoffs
    with open(outdir + os.sep + 'cdf_cutoffs.pck', 'rb') as handle:
        out_cdf = pickle.load(handle)

    # # 2. Add admission probabilities to each application
    # yr, inpath = '2020', '/home/tlarroucau/Dropbox/Mistakes/Code/Python/Cartillas/'

    with open(dropbox_dir + os.sep + 'outputs' + os.sep + 'cutoffs_2019_2020.pck', 'rb') as handle:
        cutoffs_19_20 = pickle.load(handle)

    # students = ReadApplications(dropbox_dir + os.sep + "data"+ os.sep + yr + os.sep + 'Reporte_diario_postulaciones_final_mrun.csv', 0, 3, False, '2021', ',')
    students = ReadApplications(dropbox_dir + os.sep + "data"+ os.sep + yr + os.sep + 'Reporte_diario_postulaciones_last_mrun.csv', 0, 3, False, '2021', ',')

    bea = ReadBEA(dropbox_dir + os.sep + "data"+ os.sep + yr + os.sep + '15-01-21_Preseleccionados_BEA_DEMRE_MRUN.csv')
    for id_s in students:
        if id_s in bea:
            students[id_s]['bea'] = True

    # CHECK OBS
    # len(students)

    for id_s in students:
        for p in students[id_s]['pref']:
            if students[id_s]['pref'][p]['cc'] == 0:
                continue
            id_c = students[id_s]['pref'][p]['cc']
            puntpond = int(math.floor(students[id_s]['pref'][p]['pp']/100))
            if puntpond <= 200:
                prob = 0
            else:
                if students[id_s]['bea']:
                    prob = out_cdf[id_c]['bea'][puntpond]
                else:
                    prob = out_cdf[id_c]['reg'][puntpond]
            students[id_s]['pref'][p]['prob'] = prob

    # f = open(outdir + os.sep + 'applications_and_probs.csv', 'w')
    f = open(outdir + os.sep + 'applications_and_probs_last.csv', 'w')
    f.write("MRUN;codigo_carrera_1;marca_1;puntpond_1;orden_1;puntano_1;prob_1;prob_level_1;codigo_carrera_2;marca_2;puntpond_2;orden_2;puntano_2;prob_2;prob_level_2;codigo_carrera_3;marca_3;puntpond_3;orden_3;puntano_3;prob_3;prob_level_3;codigo_carrera_4;marca_4;puntpond_4;orden_4;puntano_4;prob_4;prob_level_4;codigo_carrera_5;marca_5;puntpond_5;orden_5;puntano_5;prob_5;prob_level_5;codigo_carrera_6;marca_6;puntpond_6;orden_6;puntano_6;prob_6;prob_level_6;codigo_carrera_7;marca_7;puntpond_7;orden_7;puntano_7;prob_7;prob_level_7;codigo_carrera_8;marca_8;puntpond_8;orden_8;puntano_8;prob_8;prob_level_8;codigo_carrera_9;marca_9;puntpond_9;orden_9;puntano_9;prob_9;prob_level_9;codigo_carrera_10;marca_10;puntpond_10;orden_10;puntano_10;prob_10;prob_level_10;overall_prob\n")
    for id_s in students:
        st = str(id_s)
        overall, neg, red = 1,1,0
        for p in range(1,11):
            if p == 1:
                overall = round(students[id_s]['pref'][p]['prob'],3)
            else:
                overall += neg*round(students[id_s]['pref'][p]['prob'],3)
            neg *= (1-round(students[id_s]['pref'][p]['prob'],3))

            id_c = students[id_s]['pref'][p]['cc']
            if id_c not in cutoffs_19_20:
                cutoffs_19_20[id_c] = {'2019':{'max':0, 'min':0, 'full':0}, '2020':{'max':0, 'min':0, 'full':0}}
            if id_c != 0:
                if students[id_s]['pref'][p]['prob'] < 0.01 and students[id_s]['pref'][p]['pp'] < min(cutoffs_19_20[id_c]['2019']['min'], cutoffs_19_20[id_c]['2020']['min']):
                    red = 1
                else:
                    red = 0
            else:
                red = 0
            st += ";"+str(students[id_s]['pref'][p]['cc'])+";"+str(students[id_s]['pref'][p]['mc'])+";"+str(students[id_s]['pref'][p]['pp'])+";"+str(students[id_s]['pref'][p]['od'])+";"+str(students[id_s]['pref'][p]['pa'])+";"+str(round(students[id_s]['pref'][p]['prob'],3))+";"+str(red)
        f.write(st+';'+str(round(overall, 3))+"\n")
    f.close()
