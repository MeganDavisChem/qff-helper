import qff_helper

helper = qff_helper.QffHelper()

helper.setup_spectro_dir('example_files/hcf/files/freqs/',
        'example_files/hcf/pts/intder.in')

helper.make_relative_energies('example_files/energy.dat')

helper.run_anpass()
