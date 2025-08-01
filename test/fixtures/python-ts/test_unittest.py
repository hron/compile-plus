import unittest

class TestStringMethods(unittest.TestCase):

    def test_upper(self):
        self.assertEqual('foo'.upper(), 'FOO')


class SomeClassWithoutPrefix(unittest.TestCase):

    def test_add(self):
        self.assertEqual(2 + 2, 4)
