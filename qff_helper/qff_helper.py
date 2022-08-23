"""Main module."""
import os


def make_relative_energies(str: _energy_file):
    """Converts energy.dat file to relative energies"""
    return _relative_energies

def run_anpass(_rel_energies):
    """Passes relative energies into anpass"""
    pass

def run_intder_geom():
    """Reads anpass2.out and runs intder_geom"""
    pass

def run_intder():
    """Reads intder_geom.out and runs intder"""
    #cpfort stuff
    pass

def run_spectro():
    """Reads cartesian force constants and runs spectro"""
    pass

def setup_spectro_dir(str: _files_dir, _intder_geom_file):
    """Takes input directory with spectro.tmp, anpass.tmp and intder.tmp as well
    as location of intder.in for pts
    Makes a structured freqs directory with em
    """
    pass

def run_summarize():
    """Runs summarize program after spectro is output"""
    pass

def run_spec_to_latex():
    """Does spectro to latex stuff"""
    pass

def auto_spec(str: _files_dir, str: _intder_geom_file, str: _energy_file):
    """Does the whole spectro process...automatically!!!"""
    setup_spectro_dir(_files_dir, _intder_geom_file)
    _relative_energies = make_relative_energies(_energy_file)
    run_anpass(_relative_energies)
    run_intder_geom()
    run_intder()
    run_spectro()
    run_summarize()
    spec_to_latex()




