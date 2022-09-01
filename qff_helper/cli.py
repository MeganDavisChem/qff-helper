"""Console script for qff_helper."""
import sys
import click
import qff_helper
import qff_helper.qff_helper as qffh

@click.command()
@click.argument("files_dir") 
@click.argument("intder_geom_file")
@click.argument("energy_dat")
@click.argument("molecule")
@click.argument("theory")
@click.argument("freqs_dir")

def main(files_dir, intder_geom_file, energy_dat, molecule, theory, freqs_dir="freqs"):
    """Console script for qff_helper."""
    helper = qffh.QffHelper(freqs_dir)
    helper.auto_spec(files_dir, intder_geom_file, energy_dat)
    helper.run_spec_to_latex(theory, molecule)


if __name__ == "__main__":
    sys.exit(main())  # pragma: no cover
