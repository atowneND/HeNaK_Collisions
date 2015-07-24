#!/usr/bin/python
# pyplot.py

import matplotlib.pyplot as plt
from matplotlib.patches import Ellipse
import numpy
import math
from math import cos, sin, tan, atan, pi, sqrt

class plotComponents:
    def __init__(self, **kwargs):
        self.__dict__.update(**kwargs)

    def set_ellipses(
        self,
        plot_handle,
    ):
        j_x = 0.
        j_y = 2.
        j_prime_x = 0.
        j_prime_y = 1.
        r_alpha = .5

        phi_y = 1.5
        phi_x = phi_y/4

        slope = atan(4)*180/pi + 90

        j_ellipse = Ellipse(xy=(j_x,j_y), width = j_y/2, height = .5, edgecolor = 'r', fc = 'None', lw = 2)
        j_prime_ellipse = Ellipse(xy=(j_prime_x,j_prime_y), width = j_prime_y/2, height = .25, edgecolor = 'b', fc = 'None', lw = 1)
        phi_ellipse = Ellipse(xy=(phi_x,phi_y), width = r_alpha, height = .25, angle = slope, edgecolor = 'g', fc = 'None', lw = 1)

        plot_handle.add_patch(j_ellipse)
        plot_handle.add_patch(j_prime_ellipse)
        plot_handle.add_patch(phi_ellipse)
        plot_handle.set_aspect('equal')

    def set_j_vectors(
        self,
        plot_handle,
    ):
        j_x = .5
        j_y = 2
        j_prime_x = .25
        j_prime_y = 1.5
        all_vectors= numpy.array(
            [ [0,0,j_x,j_y],
            [0,0,j_prime_x,j_prime_y] ]
        )
        X,Y,U,V = zip(*all_vectors)
        return [X,Y,U,V] 
    
    def plot_data( 
        self
    ):
        plot_handle = plt.gca()

        self.set_ellipses(plot_handle)

        X,Y,U,V=self.set_j_vectors(plot_handle)
        plot_handle.quiver(X,Y,U,V, angles='xy',scale_units='xy',scale=1)

        plt.show()

    def get_ellipse_coords(a=0.0, b=0.0, x=0.0, y=0.0, angle=0.0, k=2):
        """ Draws an ellipse using (360*k + 1) discrete points; based on pseudo code
        given at http://en.wikipedia.org/wiki/Ellipse
        k = 1 means 361 points (degree by degree)
        a = major axis distance,
        b = minor axis distance,
        x = offset along the x-axis
        y = offset along the y-axis
        angle = clockwise rotation [in degrees] of the ellipse;
        * angle=0  : the ellipse is aligned with the positive x-axis
        * angle=30 : rotated 30 degrees clockwise from positive x-axis
        """
        pts = numpy.zeros((360*k+1, 2))

        beta = -angle * pi/180.0
        sin_beta = sin(beta)
        cos_beta = cos(beta)
        alpha = numpy.radians(numpy.r_[0.:360.:1j*(360*k+1)])

        sin_alpha = sin(alpha)
        cos_alpha = cos(alpha)

        pts[:, 0] = x + (a * cos_alpha * cos_beta - b * sin_alpha * sin_beta)
        pts[:, 1] = y + (a * cos_alpha * sin_beta + b * sin_alpha * cos_beta)

        return pts

    def get_point_on_ellipse(
        self,
        theta,
        x_center,
        y_center,
        ellipse_height,
        ellipse_width,
    ):
        cos_theta = cos(theta)
        sin_theta = sin(theta)
        tan_theta = sin_theta / cos_theta  ## tan(theta)
        tan_t = tan_theta * ellipse_width / ellipse_height  ## tan(t)
        distance = 1. / sqrt(1. + tan_t * tan_t)
        x = x_center + math.copysign(ellipse_width * distance, cos_theta)
        y = y_center + math.copysign(ellipse_height * tan_t * distance, sin_theta)
        return x, y

    def get_point_on_tilted_ellipse(
        self,
        theta,
        x_center,
        y_center,
        ellipse_height,
        ellipse_width,
        phi,
    ):
        """
        find point at angle theta on ellipse tilted by angle phi
        """
        a = ellipse_width
        b = ellipse_height
        foo = (cos(phi)**2 + 2*cos(phi)*sin(phi)*tan(theta) + sin(phi)**2*tan(theta)**2)/(a**2)+(sin(phi)**2 + 2*sin(phi)*cos(phi)*tan(theta) + cos(phi)**2*tan(theta)**2)/(b**2)
        x=1./sqrt(foo)
        y=x*tan(theta)
        return x,y,-x,-y

if __name__=='__main__':
    dma = plotComponents()
    #dma.plot_data()
    xtvec=[]
    ytvec=[]
    xtpvec=[]
    ytpvec=[]
    xvec=[]
    yvec=[]
    foox=[]
    fooy=[]
    for j in xrange(0,10):
        phi = j*pi/11
        print phi*180/pi

        xvec=[]
        yvec=[]
        foox=[]
        fooy=[]

	for k in xrange(0,101):
	    theta = k*pi/50
	    xt,yt,xtp,ytp=dma.get_point_on_tilted_ellipse(theta,0.,0.,2.,3.,phi)
	    x,y=dma.get_point_on_ellipse(theta,0.,0.,1.,1.)
	    xvec.append(x)
	    yvec.append(y)
	    foox.append(xt)
	    foox.append(xtp)
	    fooy.append(yt)
	    fooy.append(ytp)
	
	plt.plot(
	    foox,
	    fooy,
	    '.'
	)
	plt.plot(
	    xvec,
	    yvec,
	)
        plot_handle = plt.gca()
        fooellipse = Ellipse(xy=(0,0), width = 6, height = 4, angle = 180-phi*180/pi, edgecolor = 'r', fc = 'None', lw = 1)
        plot_handle.add_patch(fooellipse)
	plt.show()
