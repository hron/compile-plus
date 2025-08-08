import unittest

class TestStringMethods(unittest.TestCase):

    def test_capitalize(self):
        self.assertEqual('foo'.capitalize(), 'Foo')

    def test_upper(self):
        self.assertEqual('foo'.upper(), 'FOO')


class SomeClassWithoutPrefix(unittest.TestCase):

    def test_add(self):
        self.assertEqual(2 + 2, 4)

    def test_sub(self):
        self.assertEqual(4 - 2, 2)
