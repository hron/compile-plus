class TestPytestClass:

    def test_method(self):
        assert 'foo'.upper() == 'FOO'


def test_function():
    assert 2 + 2 == 4


def doctest():
    """
    >>> 2+3
    4
    """
