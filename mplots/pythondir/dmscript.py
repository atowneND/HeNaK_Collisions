#!/usr/bin/python
# dmscript.py
# MAINTAINER: Baetowne

import logging
import subprocess
from tempfile import NamedTemporaryFile
import os

import matplotlib.pyplot as plt
import numpy


logger = logging.getLogger('data_model_analysis_logger')
logging.basicConfig(level=logging.INFO)

class DataModelAnalysis:

    def __init__(self, **kwargs):
        self.__dict__.update(**kwargs)

    j = None
    j_prime = None
    initial_raw_data = None

    def normalize_data(self, y_values, bin_width):
        y_total = sum(y_values)
        y_normalized = y_values/(y_total*bin_width)
        return y_normalized

    def plot_data(self):
        """
            Plots semiclassical data
            and quantum mechanical histogram data
        """
        qm_data = self.calculate_quantum_bins_histogram()
        semiclassical_data = self.process_semiclassical_data()

        plt.plot(
            semiclassical_data[0],
            semiclassical_data[1],
            'rs',
            label='Semiclassical data',
        )
        plt.plot(
            qm_data[1],
            qm_data[0],
            'b--',
            label='Quantum data',
        )


        plt.legend(loc='upper center')
        plt.show()

    def calculate_quantum_bins_histogram(self):
        """
            figures out quantum bins

            returns [array(values), array(bin_edges)]
        """
        bins = 720
        bin_width = .5

        if not self.j:
            raise IndexError('j not set')

        if not self.j_prime:
            raise IndexError('j_prime not set')

        dj = self.j_prime - self.j

        quantum_model_data_path = (
            'qmthetasHe/qmth_j{j}_jp{j_prime}_dj{dj}.dat'.format(
                j=self.j,
                j_prime=self.j_prime,
                dj=dj,
            )
        )

        x_values = []
        weights = []
        with open(quantum_model_data_path, "r") as f:
            for line in f.readlines():

                # Skip over comment
                if line[0] == '#':
                    continue

                tokens = line.split()
                x_values.append(float(tokens[2]))
                weights.append(float(tokens[3]))
        
        hist = numpy.histogram(
            x_values,
            bins=bins,
            weights=weights,
        )

        # Calculate average of bin edges
        bin_edges = hist[1]
        avg_bin_edges = [
            (bin_edges[i]+bin_edges[i+1])/2
            for i in xrange(0,len(bin_edges) - 1)]

        # normalize data
        y_normalized = self.normalize_data(hist[0], bin_width)

        return [y_normalized, avg_bin_edges]

    def weighted_angle_hist(self):
        """
            Makes histogram of cross-sections
            as a function of the change in theta.
            First gets semiclassical data from 
            fortran function. 

            dj: deltaj
            j: part of quantum number
        """
        if not self.quantum_model_data_path:
            raise IndexError('quantom_model_data_path not set')

    def process_semiclassical_data(
        self,
        theta_bin=1./2,
        alpha_bin=1./2,
        phi_bin=1./2,
    ):
        """
            Processes intial_raw_data
            and calculates semiclassical data
            for plotting
        """
        f = NamedTemporaryFile("w+")

        if not self.initial_raw_data:
            raise IndexError('initial_raw_data not set')

        logger.info("Using dthetabar to calculate data...")
        status_code = subprocess.check_call((
            '../dthetascquad '
            '{initial_raw_data} {semi_data_path} {j} {j_prime}'
            ).format(
                initial_raw_data=self.initial_raw_data,
                semi_data_path=f.name,
                j=self.j,
                j_prime=self.j_prime,
                ),
            shell=True,
        )

        if status_code == 0:
            logging.info("Data calculation returned 0")
        else:
            logging.warning("Data calculation returned 1")


        x_values = []
        y_values = []
        for line in f.readlines():
            stripped_line = line.lstrip()
            if stripped_line[0] == '#':
                continue
            tokens = stripped_line.split()
            x_values.append(tokens[0])
            y_values.append(tokens[1])
            
        return [x_values, y_values]


if __name__ == '__main__':
    initial_path = "../newdata/Ep002mf5i-50b.blam"
    dma = DataModelAnalysis(
        initial_raw_data=os.path.abspath(initial_path),
        j=28,
        j_prime=32,
    )
    dma.plot_data()
