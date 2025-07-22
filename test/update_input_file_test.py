
# -*- coding: mbcs -*-
# Do not delete the following import lines
from abaqus import *
from abaqusConstants import *
import __main__

def updateinp(model_name, path_with_filename):
    import section
    import regionToolset
    import displayGroupMdbToolset as dgm
    import part
    import material
    import assembly
    import step
    import interaction
    import load
    import mesh
    import optimization
    import job
    import sketch
    import visualization
    import xyPlot
    import displayGroupOdbToolset as dgo
    import connectorBehavior
    del mdb.models[model_name]
    a = mdb.models['Model-1'].rootAssembly
    session.viewports['Viewport: 1'].setValues(displayedObject=a)
    mdb.ModelFromInputFile(name=model_name, 
        inputFileName=path_with_filename)
    a = mdb.models[model_name].rootAssembly
    session.viewports['Viewport: 1'].setValues(displayedObject=a)


model_name = 'Verco_diaphragm_36-6_18g_r1'
path_with_filename = '/enc/uprod_vNTzbg/storage_poDtd/Verco_Vulcraft/diaphragm_FEA_2025/FEA/model_runs/r1/Verco_diaphragm_36-6_18g_r1.inp'
updateinp(model_name, path_with_filename)