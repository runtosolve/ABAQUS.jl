# -*- coding: mbcs -*-
# Do not delete the following import lines
from abaqus import *
from abaqusConstants import *
import __main__

def get_connector_forces(odb_filename, output_filename):
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
    o1 = session.openOdb(
        name=odb_filename)
    session.viewports['Viewport: 1'].setValues(displayedObject=o1)
    odb = session.odbs[odb_filename]
    session.writeFieldReport(fileName=output_filename, append=ON, 
        sortItem='Element Label', odb=odb, step=0, frame=1, 
        outputPosition=WHOLE_ELEMENT, variable=(('CTF', WHOLE_ELEMENT), ), 
        stepFrame=ALL)


