use std::iter::FromIterator;
use std::mem;
use std::sync::Arc;

//TODO: Replace Arc with a template
//parameter. But that is difficult to
//do without higher-kinded-types or similar
#[derive(Debug, PartialEq, Eq)]
pub enum List<T> {
    // better names?
    Nil,
    Cons(Arc<(T, List<T>)>)
}

use self::List::*;

// implement this ourselves because of https://github.com/rust-lang/rust/issues/26925
impl<T> Clone for List<T> {
    fn clone(&self) -> Self {
        match *self {
            Nil => Nil,
            Cons(ref r) => Cons(Arc::clone(r))
        }
    }
}

impl<T> List<T> {
    pub fn new() -> Self {
        Nil
    }

    pub fn one(el: T) -> Self {
        Cons(Arc::new((el, Nil)))
    }

    pub fn prepend(&self, el: T) -> Self {
        Cons(Arc::new((el, self.clone())))
    }

    pub fn head(&self) -> Option<&T> {
        match *self {
            Cons(ref inner) => Some(&inner.0),
            Nil => None
        }
    }

    pub fn tail(&self) -> Self {
        match *self {
            Cons(ref inner) => inner.1.clone(),
            Nil => Nil
        }
    }

    pub fn split(&self) -> Option<(&T, Self)> {
        use self::List::*;
        match *self {
            Cons(ref inner) => Some((&inner.0, inner.1.clone())),
            Nil => None
        }
    }

    #[inline]
    pub fn iter(&self) -> Iter<T> {
        Iter {
            inner: self
        }
    }

    pub fn reversed(&self) -> Self where T: Clone {
        let mut res = Nil;
        let mut node = self;
        while let Cons(ref cons) = *node {
            res = res.prepend(cons.0.clone());
            node = &cons.1;
        }
        res
    }

    pub fn into_reversed(self) -> Self where T: Clone {
        let mut head = Nil;
        let mut node = self;
        while let Cons(mut cons) = node {
            node = mem::replace(&mut Arc::make_mut(&mut cons).1, head);
            head = Cons(cons);
        }
        head
    }

    /// Panics if we don't own every cell of the list.
    fn build_reversed(self) -> Self {
        let mut head = Nil;
        let mut node = self;
        while let Cons(mut cons) = node {
            node = mem::replace(&mut Arc::get_mut(&mut cons).unwrap().1, head);
            head = Cons(cons);
        }
        head
    }
}

impl<T> IntoIterator for List<T> where T: Clone {
    type Item = T;
    type IntoIter = IntoIter<T>;

    fn into_iter(self) -> IntoIter<T> {
        IntoIter {
            inner: self
        }
    }
}

pub struct ListBuilder<T> {
    inner: List<T>
}

impl<T> ListBuilder<T> {
    pub fn new() -> Self {
        ListBuilder {
            inner: Nil
        }
    }

    pub fn push(&mut self, item: T) -> &mut Self {
        let c = cons(item, mem::replace(&mut self.inner, Nil));
        self.inner = c;
        self
    }

    pub fn build(self) -> List<T> {
        self.inner.build_reversed()
    }

}

impl<T> FromIterator<T> for List<T> {
    fn from_iter<I>(iter: I) -> Self where I: IntoIterator<Item=T> {
        iter.into_iter().fold(Nil, |l, it| cons(it, l)).build_reversed()
    }

}

#[inline]
pub fn cons<T>(head: T, tail: List<T>) -> List<T> {
    tail.prepend(head)
}

#[derive(Debug)]
pub struct Iter<'a, T> where T: 'a {
    inner: &'a List<T>
}

impl<'a, T> Iterator for Iter<'a, T> where T: 'a {
    type Item = &'a T;
    fn next(&mut self) -> Option<&'a T> {
        match *self.inner {
            Cons(ref cons) => {
                self.inner = &cons.1;
                Some(&cons.0)
            }
            Nil => None
        }
    }
}

#[derive(Debug)]
pub struct IntoIter<T> where T: Clone {
    inner: List<T>
}

impl<T> Iterator for IntoIter<T> where T: Clone {
    type Item = T;

    fn next(&mut self) -> Option<T> {
        match mem::replace(&mut self.inner, Nil) {
            Cons(cons) => {
                let (head, tail) = Arc::try_unwrap(cons).unwrap_or_else(|p| (*p).clone());
                self.inner = tail;
                Some(head)
            }
            Nil => None
        }
    }
}

#[macro_export]
macro_rules! clist {
    () => {
        Nil
    };
    ($item:expr) => {
        Nil.prepend($item)
    };
    ($item:expr, $($items:expr),+) => {
        clist!($($items),+).prepend($item)
    };
}

#[test]
fn my_test() {
    let l1 = clist!(1,2,3);
    let l2 = cons(1, cons(2, cons(3, Nil)));
    assert_eq!(l1, l2);

    assert_eq!(clist!(), Nil::<u16>);
}


