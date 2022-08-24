"""Main module."""
import os
import shutil


class QffHelper:
    def __init__(self):
        pass

    files = {}

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
        for file in self.files.values():
            shutil.copy(file, _freqs_dir)

    def make_relative_energies(self, _energy_file: str):
        """Converts energy.dat file to relative energies"""
        with open(_energy_file) as f:
            energies = [float(energy.strip().split()[-1]) for energy in f.readlines()]
        min_energy = min(energies)
        _relative_energies = [energy - min_energy for energy in energies]
        return _relative_energies

    def run_anpass(_rel_energies):
        """Passes relative energies into anpass"""
        pass

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
