"""Main module."""
import os
import shutil
import subprocess


class QffHelper:
    def __init__(self, _freqs_dir: str = "freqs"):
        # TODO setup optional initializations with sane defaults
        self.freqs_dir = _freqs_dir
        self.freqs_files = {
            "anpass": _freqs_dir + "/anpass.tmp",
            "intder": _freqs_dir + "/intder.tmp",
            "spectro": _freqs_dir + "/spectro.tmp",
        }
        self.input_files = {
            "anpass1": _freqs_dir + "/anpass1.in",
            "anpass2": _freqs_dir + "/anpass2.in",
            "intder_geom": _freqs_dir + "/intder_geom.in",
            "intder": _freqs_dir + "/intder.in",
            "spectro": _freqs_dir + "/spectro.in",
        }
        self.output_files = {
            "anpass1": _freqs_dir + "/anpass1.out",
            "anpass2": _freqs_dir + "/anpass2.out",
            "intder_geom": _freqs_dir + "/intder_geom.out",
            "intder": _freqs_dir + "/intder.out",
            "spectro": _freqs_dir + "/spectro.out",
        }

    files = {}
    freqs_files = {}
    energies = []
    relative_energies = []
    freqs_dir = "freqs"
    anpass_refit_energy = 0.0

    #    top_location =   "/home/lailah/Documents/OleMiss/programming/Python/qff_helper/Programs"
    #    anpass_location = "/home/megan/Programs/anpass/anpass_cerebro.x"
    #    spectro_location = "/home/megan/Programs/spec3jm.ifort-00.static.x"
    #    intder_location = "/home/megan/Programs/intder/Intder2005.x"
    #    anpass_location = top_location + "/anpass/anpass_cerebro.x"
    #    spectro_location = top_location + "/spec3jm.ifort-00.static.x"
    #   intder_location = top_location = "/intder/Intder2005.x"
    programs = {
        "anpass": "/home/megan/Programs/anpass/anpass_cerebro.x",
        "spectro": "/home/megan/Programs/spec3jm.ifort-00.static.x",
        "intder": "/home/megan/Programs/intder/Intder2005.x",
    }

    def setup_spectro_dir(self, _files_dir: str, _intder_geom_file: str):
        """Takes input directory with spectro.tmp, anpass.tmp and intder.tmp as well
        as location of intder.in for pts
        Makes a structured freqs directory with em
        """
        # TODO handle matdisp.pl stuff, but later
        try:
            with open(_intder_geom_file) as intder_file:
                intder_lines = intder_file.readlines()
            disp_index = [
                i for i, line in enumerate(intder_lines) if "DISP" in line
            ].pop()
            int_geom_header = intder_lines[0:disp_index]
            #TODO might need to fix spacing here
            int_geom_header.append("DISP 1")
        except:
            print("Could not open inter_geom file")

        if not os.path.exists(self.freqs_dir):
            os.makedirs(self.freqs_dir)
        with open(self.freqs_dir + "/intder_geom.in", "w") as int_geom_file:
            for line in int_geom_header:
                int_geom_file.write(line)
        self.files = {
            "anpass": _files_dir + "/anpass.tmp",
            "intder": _files_dir + "/intder.tmp",
            "spectro": _files_dir + "/spectro.tmp",
        }

        for file in self.files.values():
            shutil.copy(file, self.freqs_dir)

    def make_relative_energies(self, _energy_file: str):
        """Converts energy.dat file to relative energies"""
        try:
            with open(_energy_file) as f:
                energies = [
                    float(energy.strip().split()[-1]) for energy in f.readlines()
                ]
        except:
            print("improperly formatted energy.dat file")
        self.energies = energies
        min_energy = min(energies)
        self.relative_energies = [energy - min_energy for energy in energies]
        return self.relative_energies

    def run_anpass(self):
        """Passes relative energies into anpass"""
        # paste the relative energies into the properly formatted anpass file
        
        #this is all to do with setting up anpass1.in
        anpass_temp = self.freqs_files['anpass']
        anpass1_in = self.input_files['anpass1']
        anpass1_out = self.output_files['anpass1']
        anpass2_in = self.input_files['anpass2']
        anpass2_out = self.output_files['anpass2']

        with open(anpass_temp) as anpass_temp:
            anpass_lines = [line.strip("\n") for line in anpass_temp.readlines()]

        datapoint_line_index = self.find_index(anpass_lines, 'DATA POINTS')

        anpass_header = anpass_lines[0 : datapoint_line_index + 3]

        number_of_points = int(anpass_header[-2].split()[0])
        number_of_coordinates = int(anpass_header[-4])

        start_points_index = datapoint_line_index + 3
        end_points_index = number_of_points + start_points_index

        anpass_points = anpass_lines[start_points_index:end_points_index]
        # probably a better way to do this
        anpass_footer = [line for line in anpass_lines[end_points_index:] if line != ""]

        # now to start processing the points
        anpass_coords = [
            [float(coord) for coord in line.split()[:-1]] for line in anpass_points
        ]

        for i in range(len(anpass_coords)):
            anpass_coords[i].append(self.relative_energies[i])

        header_string = "\n".join(anpass_header)
        footer_string = "\n".join(anpass_footer)

        coord_string_template = "%12.8f " * number_of_coordinates + "%20.12f"
        coord_string = "\n".join(
            [coord_string_template % (tuple(line)) for line in anpass_coords]
        )


        #this is where we actually write anpass1
        with open(anpass1_in, "w") as f:
            f.write(header_string)
            f.write("\n")
            f.write(coord_string)
            f.write("\n")
            f.write(footer_string)

        #this is where we actually run anpass
        self.run_program('anpass1')

        # now do anpass2
        with open(anpass1_out) as f:
            anpass1_lines = f.readlines()

        #This is all unique to anpass2
        energy_index = self.find_index(anpass1_lines, "WHERE ENERGY IS")

        self.anpass_refit_energy = float(
            anpass1_lines[energy_index].strip("\n").split()[-1]
        )

        anpass_refit_coords = anpass1_lines[
            energy_index + number_of_coordinates + 1
        ].strip("\n")

        self.anpass_refit_coords = anpass_refit_coords

        # find stationary point from output file

        # handle stationary points
        anpass_footer.pop(-2)
        anpass_footer.insert(-3, "STATIONARY POINT")
        anpass_footer.insert(-3, anpass_refit_coords)

        # reform footer string
        footer_string = "\n".join(anpass_footer)

        with open(anpass2_in, "w") as f:
            f.write(header_string)
            f.write("\n")
            f.write(coord_string)
            f.write("\n")
            f.write(footer_string)

        # now run anpass2.out
        self.run_program('anpass2')
        # TODO split run anpass and read anpass into separate functions? yes

    def find_index(self, _list_of_lines, _search_string):
        """Gets the index of a search string for a list of lines"""
        return [
            i for i, line in enumerate(_list_of_lines) if _search_string in line
        ].pop()

    def run_intder_geom(self):
        """Reads anpass2.out and runs intder_geom"""

        # TODO do this with try/except and make it more elegant as a standalone  fn
        intgeom_coords = self.anpass_refit_coords.strip().split()[:-1]

        # TODO definitely refactor this code to store all these
        intgeom_file = self.freqs_dir + "/intder_geom.in"

        with open(intgeom_file, "a") as f:
            for i, disp in enumerate(intgeom_coords):
                j = i + 1
                f.write("\n" + f"{j:5}      {disp:15}")
            f.write("\n    0")

        #with open(intgeom_file) as input_file, open(anpass2_out, "w") as out_file:
        #    subprocess.run(self.anpass_location, stdin=anpass_in, stdout=out_file)
        self.run_program('intder_geom')


    def run_program(self, _program : str):
        """Runs a program. Accepts anpass1 or anpass2 :)"""
        input_file = self.input_files[_program]
        output_file = self.output_files[_program]

        if _program == 'anpass1' or _program == 'anpass2':
            _program = 'anpass'
        elif _program == 'intder_geom':
            _program = 'intder'
        executable = self.programs[_program]

        with open(input_file) as input_file, open(output_file, "w") as output_file:
            subprocess.run(executable, stdin=input_file, stdout=output_file)

    def run_intder():
        """Reads intder_geom.out and runs intder"""
        # cpfort stuff
        pass

    def run_spectro():
        """Reads cartesian force constants and runs spectro"""
        pass

    def run_summarize():
        """Runs summarize program after spectro is output"""
        pass

    def run_spec_to_latex():
        """Does spectro to latex stuff"""
        pass

    def auto_spec(_files_dir: str, _intder_geom_file: str, _energy_file: str):
        """Does the whole spectro process...automatically!!!"""
        setup_spectro_dir(_files_dir, _intder_geom_file)
        _relative_energies = make_relative_energies(_energy_file)
        run_anpass(_relative_energies)
        run_intder_geom()
        run_intder()
        run_spectro()
        run_summarize()
        spec_to_latex()
