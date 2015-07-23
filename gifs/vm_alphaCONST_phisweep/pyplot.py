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
        x=0.
        y=0.
        a = ellipse_width
        b = ellipse_height
#        foo = (cos(phi)**2+sin(phi)**2*tan(theta**2))/(a**2) + (sin(phi)**2+cos(phi)**2*tan(theta)**2)/(b**2) + 2*tan(theta)*sin(phi)*cos(phi)*(1/(a**2)-1/(b**2))
#        print "foo =",foo
#        print "a =",(cos(phi)**2+sin(phi)**2*tan(theta))/(a**2) 
#        print "b =",(sin(phi)**2+cos(phi)**2*tan(theta)**2)/(b**2) 
#        print "   ",theta
#        print "c =",2*tan(theta)*sin(phi)*cos(phi)*(1/(a**2)-1/(b**2))
        foo = (cos(phi)**2 + 2*cos(phi)*sin(phi)*tan(theta) + sin(phi)**2*tan(theta)**2)/(a**2)+(sin(phi)**2 + 2*sin(phi)*cos(phi)*tan(theta) + cos(phi)**2*tan(theta)**2)/(b**2)
        x=1./sqrt(foo)
        y=x*tan(theta)
        return x,y

if __name__=='__main__':
    dma = plotComponents()
    #dma.plot_data()
    for k in xrange(1,101):
        theta = k*pi/100
        xt,yt=dma.get_point_on_tilted_ellipse(theta,0.,0.,1.,1.,0.)
        x,y=dma.get_point_on_ellipse(theta,0.,0.,1.,1.)
        if (abs(x-xt)>.01):
            if (abs(x)==abs(xt)) and (abs(y)==abs(yt)):
                break
            if (abs(y-yt)>.01):
                print "  ",theta*180/pi," x,y: (",xt,yt," )",x,y," )"
            else:
                print "  ",theta*180/pi," x:",xt,x
        else:
            if (abs(y-yt)>.01):
                print "  ",theta*180/pi," y:",yt,y
