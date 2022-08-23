#!/usr/bin/env python

"""Tests for `qff_helper` package."""


import unittest
from click.testing import CliRunner

from qff_helper import qff_helper
from qff_helper import cli


class TestQff_helper(unittest.TestCase):
    """Tests for `qff_helper` package."""

    def setUp(self):
        """Set up test fixtures, if any."""

    def tearDown(self):
        """Tear down test fixtures, if any."""

    def test_000_something(self):
        """Test something."""

    def test_command_line_interface(self):
        """Test the CLI."""
        runner = CliRunner()
        result = runner.invoke(cli.main)
        assert result.exit_code == 0
        assert 'qff_helper.cli.main' in result.output
        help_result = runner.invoke(cli.main, ['--help'])
        assert help_result.exit_code == 0
        assert '--help  Show this message and exit.' in help_result.output
