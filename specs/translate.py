from xml.dom import minidom
import os
def parseXml(xml_file):
    def var_strip(var):
        if var.startswith('P.') or var.startswith('C.'):
            return var[2:]
        return var
    xml = minidom.parse(xml_file)
    traces_xml = xml.getElementsByTagName('counter-example')
    traces = [None] * len(traces_xml)
    for trace_id, trace_xml in enumerate(traces_xml):
        states_xml = trace_xml.getElementsByTagName('state')
        traces[trace_id] = [None] * len(states_xml)
        for state in states_xml:
            state_id = int(state.attributes['id'].value) - 1
            traces[trace_id][state_id] = {var_strip(value.attributes['variable'].value): value.firstChild.data == 'TRUE' 
                                          for value in state.getElementsByTagName('value')}
    return traces

def get_file_content(file, util=lambda file: open(file, 'r')):
    with util(file) as f:
        return [s.strip() for s in f.readlines()]
    
input_names = get_file_content('../data/pnp/input-names')
output_names = get_file_content('../data/pnp/output-names')
    
import gzip
import shutil
def writeSpecialFormat(traces, file_name):
    with open(file_name, 'w') as f:
        f.write("{}\n".format(len(traces)))
        for trace in traces:
            for state in trace:
                if state['CNF']:
                    f.write('out=CNF[{}];  '.format(''.join(['1' if state[name] else '0' for name in output_names])))
                if state['REQ']:
                    f.write('in=REQ[{}];  '.format(''.join(['1' if state[name] else '0' for name in input_names])))
            f.write('\n')
    with open(file_name, 'rb') as f_in:
        with gzip.open(f'{file_name}.gz', 'wb') as f_out:
            shutil.copyfileobj(f_in, f_out)

def prepare_tests(folder):
    for _file in list(os.listdir(folder)):
        file = f"{folder}/{_file}"
        print(file)
        if file.endswith('xml'):
            writeSpecialFormat(parseXml(file), file[:-4])
            os.remove(file[:-4])
            os.remove(file)
            
prepare_tests('../specs/traces')
