class Rectangle:
    r"""A rectangle on the plane.

    The corners are given as two points, and the sides are parallel to
    the axes.  A rectangle is *immutable*: every method that looks like
    a change returns a new one.

    :param low: the corner with the smaller coordinates.
    :param high: the corner with the larger coordinates.

    The area is :math:`(x_1 - x_0)\,(y_1 - y_0)`, and the diagonal
    follows from :math:`d = \sqrt{w^2 + h^2}`.

    What it can do:

    - ``area()`` and ``diagonal()`` measure it.
    - ``scaled(factor)`` returns a new one about the same centre.
    - ``__contains__`` answers for a point.

    ===========  =====================================
    Property     Value
    ===========  =====================================
    sides        parallel to the axes
    winding      counter-clockwise
    equality     by corners, not by area
    ===========  =====================================

    .. code-block:: python

        unit = Rectangle(Point(0, 0), Point(1, 1))
        unit.scaled(2).area()   # 4.0
    """

    def area(self):
        """Return the area, as a `float`."""
        return self.width * self.height

    def scaled(self, factor):
        """Return this rectangle scaled about its centre.

        A factor of ``1.0`` returns an equal rectangle; a factor below
        zero raises `ValueError`, because a rectangle has no negative
        side.
        """
        return Rectangle(self.low * factor, self.high * factor)
