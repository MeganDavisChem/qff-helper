"""Main module."""
import os
import shutil
import subprocess


class QffHelper:
    def __init__(self):
        pass

    files = {}
    energies = []
    relative_energies = []
    freqs_dir = ""
    anpass_refit_energy = 0.0

    anpass_location = "/home/megan/Programs/anpass/anpass_cerebro.x"
    spectro_location = "/home/megan/Programs/spec3jm.ifort-00.static.x"
    intder_location = "/home/megan/Programs/intder/Intder2005.x"

    def setup_spectro_dir(
        self, _files_dir: str, _intder_geom_file: str, _freqs_dir: str = "freqs"
    ):
        """Takes input directory with spectro.tmp, anpass.tmp and intder.tmp as well
        as location of intder.in for pts
        Makes a structured freqs directory with em
        """
        # TODO handle matdisp.pl stuff, but later
        # TODO maybe move this to init function?
        try:
            with open(_intder_geom_file) as intder_file:
                intder_lines = intder_file.readlines()
            disp_index = [
                i for i, line in enumerate(intder_lines) if "DISP" in line
            ].pop()
            int_geom_header = intder_lines[0:disp_index]
            int_geom_header.append("DISP 1")
        except:
            print("Could not open inter_geom file")

        if not os.path.exists(_freqs_dir):
            os.makedirs(_freqs_dir)
        with open(_freqs_dir + "/intder_geom.in", "w") as int_geom_file:
            for line in int_geom_header:
                int_geom_file.write(line)
        self.files = {
            "anpass_file": _files_dir + "/anpass.tmp",
            "intder_file": _files_dir + "/intder.tmp",
            "spectro_file": _files_dir + "/spectro.tmp",
        }

        self.freqs_dir = _freqs_dir
        for file in self.files.values():
            shutil.copy(file, _freqs_dir)

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
        # TODO replace this stuff with a classwide dict
        anpass_temp = self.freqs_dir + "/anpass.tmp"

        with open(anpass_temp) as anpass_temp:
            anpass_lines = [line.strip("\n") for line in anpass_temp.readlines()]

        datapoint_line_index = [
            i for i, line in enumerate(anpass_lines) if "DATA POINTS" in line
        ].pop()

        # TODO maybe +3
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

        print(anpass_coords)
        anpass_in = self.freqs_dir + "/anpass1.in"
        print(anpass_in)

        header_string = "\n".join(anpass_header)
        footer_string = "\n".join(anpass_footer)

        coord_string_template = "%12.8f " * number_of_coordinates + "%20.12f"
        coord_string = "\n".join(
            [coord_string_template % (tuple(line)) for line in anpass_coords]
        )

        with open(anpass_in, "w") as f:
            f.write(header_string)
            f.write("\n")
            f.write(coord_string)
            f.write("\n")
            f.write(footer_string)

        # actually runs anpass (first time)
        anpass_out = self.freqs_dir + "/anpass1.out"

        with open(anpass_in) as anpass_in, open(anpass_out, "w") as out_file:
            subprocess.run(self.anpass_location, stdin=anpass_in, stdout=out_file)

        # now do anpass2
        with open(anpass_out) as f:
            anpass1_lines = f.readlines()

        energy_index = [
            i for i, line in enumerate(anpass1_lines) if "WHERE ENERGY IS" in line
        ].pop()

        self.anpass_refit_energy = float(
            anpass1_lines[energy_index].strip("\n").split()[-1]
        )

        anpass_refit_coords = anpass1_lines[
            energy_index + number_of_coordinates + 1
        ].strip("\n")

        # find stationary point from output file

        # handle stationary points
        anpass_footer.pop(-2)
        anpass_footer.insert(-3, "STATIONARY POINT")
        anpass_footer.insert(-3, anpass_refit_coords)

        # reform footer string
        footer_string = "\n".join(anpass_footer)

        # write anpass2.in
        anpass2_in = self.freqs_dir + "/anpass2.in"

        with open(anpass2_in, "w") as f:
            f.write(header_string)
            f.write("\n")
            f.write(coord_string)
            f.write("\n")
            f.write(footer_string)

    def run_intder_geom():
        """Reads anpass2.out and runs intder_geom"""
        pass

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
