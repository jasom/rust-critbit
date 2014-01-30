use std::util::swap;
use std::fmt;
use std::io;
use std::rand;
use std::os;
use std::hashmap;
use std::num;
use std::util;
use std::default::Default;
use std::uint;

trait Bitable {
    fn getb(&self, off: uint) -> u8;
    fn mismatch<'a> (&self,other : &Self) -> Option<(uint,u8,u8)> ;
}

impl Bitable for uint {
    fn getb(&self, off : uint) -> u8{
        if off >= uint::BYTES {
            0_u8
        }
        else {
             (0xff&(self >> (uint::BITS-8-(off*8)))) as u8
        }
    }
    fn mismatch(&self,other : &uint) -> Option<(uint,u8,u8)>{
        if (self == other) {return None}
        let mut shift = uint::BITS-8;
        loop {
            if ((self >> shift) != (other >> shift)) {
                return Some(((uint::BITS-shift-8)/8,
                    (0xff&(self >> shift)) as u8,
                    (0xff&(other >> shift)) as u8));
            }
            shift-=8
        }
    }
}

impl Bitable for ~str {
    fn getb(&self, off: uint) -> u8{
        let bytes = self.as_bytes();
        if(off  >= bytes.len()) {
            0x00
        }
        else {
            //str::as_bytes(s) {|bytes| bytes[off]}
            bytes[off]
        }
    }

    fn mismatch (&self,other : &~str) -> Option<(uint,u8,u8)> {
        let s1b = self.as_bytes();
        let s2b = other.as_bytes();
        let mut it = s1b.iter().zip(s2b.iter()).enumerate();
        loop
        {
            match it.next() {
                Some((count,(c1,c2))) => if(c1 != c2) {
                    //io::print(fmt!("Mismatch: %u,%u,%u\n",count,*c1 as uint, *c2 as uint));
                    return Some((count,*c1,*c2)) },
                    None() =>
                        return
                        if(self.len() > other.len()) { Some((other.len(),self[other.len()],0))}
                        else if(other.len() > self.len()) { Some((self.len(),0,other[self.len()]))}
                        else { None }
            }
        }
    }
}

struct ExternalNode<K,V> {
    key: K,
    value: V
}

struct InternalNode<K,V> {
    left: Node<K,V>,
    right: Node<K,V>,
    len: uint
}

struct CritbitTree<K,V> {
    root: Option<Node<K,V>>,
    size: uint,
}

enum Node<K,V> {
    internalNode(~InternalNode<K,V>),
    externalNode(~ExternalNode<K,V>)
}

impl<K,V> InternalNode<K,V> {
    fn pop_left(&mut self ,replace : Node<K,V>)  -> Node<K,V> {
        util::replace(&mut self.left,replace)
    }
    fn pop_right(&mut self, replace : Node<K,V>) -> Node<K,V> {
        util::replace(&mut self.right,replace)
    }
}

pub struct NodeIterator<K,V> {
    priv cur: Option<Node<K,V>>,
}

impl<K : Default,V : Default> Iterator<uint> for NodeIterator<K,V> {
    fn next(&mut self) -> Option<uint>{
        match self.cur.take() {
            Some(externalNode(_)) => None,
            None => None,
            Some(internalNode(cur)) => {
                let mut cur = cur;
                loop {
                    let dummy = externalNode(~ExternalNode {key: Default::default(), value : Default::default()});
                    match cur.pop_left(dummy) {
                        internalNode(node) => {
                            let mut node = node;
                            let dummy = externalNode(~ExternalNode {key : Default::default(), value : Default::default()});
                            cur.left = node.pop_right(dummy);
                            node.right = internalNode(cur);
                            cur = node;
                        }

                        externalNode(n) => {
                            self.cur = Some(cur.pop_right(externalNode(n)));
                            return Some(0);
                        }
                    }
                }
            },
        }
    }
}

impl<K,V> Container for CritbitTree<K,V> {
    fn len(&self) -> uint {self.size}
    fn is_empty(&self) -> bool {self.len() == 0}
}

impl<K : Default,V : Default> Mutable for CritbitTree<K,V> {
    fn clear(&mut self) {
        let mut iter = NodeIterator { cur: self.root.take()} ;
        for _ in iter {
            //ignore
        }
    }
}

fn lenToMask(len: uint) -> u8 {
    (len & 0xff) as u8
}

fn lenToOffset(len: uint) -> uint {
    len >> 8
}


fn goleft<K : Bitable> (key: &K, off: uint, mask: u8) -> bool{
    //io::print(fmt!("goleft %s,%u,%ux->%ux\n",key,off,mask as uint,(getb(key, off) & mask) as uint));
    (key.getb(off) & mask) == 0
}


impl<K : fmt::Default + Bitable + Eq + Default, V: Default> Node<K,V> {
    fn findbest<'a>(&'a self, key : &K) -> (&'a K,&'a V)
    {
        let mut cb = self;
        loop {
            let tmp = cb;
            match tmp {
                &internalNode(ref n) =>{
                    if (goleft(key,lenToOffset(n.len),lenToMask(n.len))) {
                        cb = &n.left;
                    }
                    else {
                        cb = &n.right;
                    }
                }
                &externalNode(ref n) => return (&'a n.key,&'a n.value) 
            }
        }
    }

    fn findbestMut<'a>(&'a mut self, key :&K) -> (&'a K,&'a mut V)
    {
        let mut cb = self;
        loop {
            let tmp = cb;
            match tmp {
                &internalNode(ref mut n) =>{
                    if (goleft(key,lenToOffset(n.len),lenToMask(n.len))) {
                        cb = &mut n.left;
                    }
                    else {
                        cb = &mut n.right;
                    }
                }
                &externalNode(ref mut n) => {
                    return (&'a n.key, &'a mut n.value)
                }
            }
        }
    }

    fn insertAtPoint<'a>(&'a mut self, key : K, value : V, count: uint, mask :u8)
    {
        let cb=self;
        let direction = goleft(&key,count,mask);
        let newExtNode = externalNode(~ExternalNode { key: key, value : value});
        let mut tmp = externalNode(~ExternalNode { key : Default::default(), value : Default::default()});
        swap(&mut tmp,cb);
        let mut newIntNode = 
            if(direction) {
                internalNode(~InternalNode {left: newExtNode,
                right: tmp,
                len: (count << 8) | mask as uint})
            }
            else {
                internalNode(~InternalNode { left: tmp,
                right: newExtNode,
                len: (count << 8) | mask as uint})
            };
        swap(&mut newIntNode,cb);
    }

    fn findInsertionPoint<'a>(&'a mut self,key : K, value: V,
                              count : uint, mask : u8)
    {
        let cb = self;
        match *cb {
            externalNode(_) => 
                cb.insertAtPoint(key,value,count,mask),

                internalNode(~InternalNode { left : _, right : _, len: len}) 
                    if(lenToOffset(len) > count ||
                       (lenToOffset(len) == count &&
                        lenToMask(len) < mask))
                        => 
                            cb.insertAtPoint(key,value,count,mask),
                            internalNode(~InternalNode{ left : _, right : _, len: len}) =>
                            {
                                let direction = goleft(&key,lenToOffset(len),lenToMask(len));
                                cb.findInsertionPointInternal(key,value,count,mask,direction)
                            }
        }
    }

    fn isInsertionPoint<'n>(&'n self, count : uint, mask : u8) -> bool {
        let cb = self;
        match cb{
            &externalNode(_) => true,
            &internalNode(ref cb) => {
                if (lenToOffset(cb.len) > count) ||
                    (lenToOffset(cb.len) == count &&
                     lenToMask(cb.len) < mask) {
                        true
                    }
                else {
                    false
                }
            }
            /*
                match if (direction) { &'n cb.left } else {&'n cb.right} {
                    &externalNode(_) => true,
                    &internalNode(ref n)
                        if(lenToOffset(n.len) > count) ||
                            (lenToOffset(n.len) == count &&
                             lenToMask(n.len) < mask) => true,
                             _ => false
                }
            */
            }
    }

    fn findInsertionPointInternal(&mut self, key : K, value: V, count : uint,
                                  mask : u8, mut direction : bool)
    {
        let mut cb = self;
            loop {
                let tmp = cb;
                if(tmp.isInsertionPoint(count,mask)) {
                    return tmp.insertAtPoint(key,value,count,mask)
                }
                else {
                    match tmp {
                        &externalNode(_) => {
                            fail!();
                        }
                        &internalNode(ref mut cbi) =>
                        {

                            let child = if goleft(&key,lenToOffset(cbi.len),lenToMask(cbi.len)) { &mut cbi.left }
                            else { &mut cbi.right };
                            cb = child;
                        }
                    }
                }
            }
    }


    fn addnew(&mut self,key : K,value : V,count : uint, mask : u8) {
        let cb = self;
        cb.findInsertionPoint(key,value,count,mask)
    }

    fn isDeletionPoint(&self, key : &K) -> (bool,bool) {
        return match *self {
            internalNode(ref internal) => { 
                let direction =  goleft(key,lenToOffset(internal.len),lenToMask(internal.len));
                let child = if direction { &internal.left } else { &internal.right };
                match *child {
                    externalNode(_) => (true,direction),
                    internalNode(_) => (false,direction),
                }
            },
            _ => fail!()
        }
    }

    fn deleteChild(&mut self, key : &K,direction : bool) -> Option<V>{
        let mut tmp =
            externalNode(~ExternalNode{key : Default::default(), value : Default::default()});
        swap(self, &mut tmp);
        match tmp {
            internalNode(ref mut n) => {
                let (die,live) = if(direction) { (&mut n.left,&mut n.right) }
                else {(&mut n.right,&mut n.left)};
                match *die {
                        externalNode(ref mut n) => {
                            if(n.key == *key) {
                                swap(self,live);
                                return Some(util::replace(&mut n.value,Default::default()));
                            }
                        },
                        _ => fail!(),
                }
            },
            _ => fail!(),
        }
        swap(self,&mut tmp);
        return None;
    }

    /* TODO test delete */
    // Note must only be called with extnernal node
    fn del(&mut self, key : &K) -> Option<V>{
        let mut n = self;
        //let mut tmp2 = externalNode(~ExternalNode{key : Default::default(), value : Default::default()});
        loop {
            let mut exit=false;
            let tmp = n;
            let (isdel,direction) = tmp.isDeletionPoint(key);
            if isdel {
                return tmp.deleteChild(key,direction);
            }
            else {
                match *tmp {
                    internalNode(ref mut internal) => {
                        let child = if direction { &mut internal.left } else { &mut internal.right };
                        n=child;
                    },
                    _ => fail!(),

                }
            }
        }
    }


}

impl <K : fmt::Default + Eq + Bitable + Default,V : Default + fmt::Default>  CritbitTree<K,V> {
    fn set(&mut self, key : K, value : V) -> Option<V> {
        //printTree(self);
        //io::print("-----\n");
        match self.root {
            Some(ref mut node) =>
            {
                let (len,mask) = {
                    let (bestkey,val) = node.findbestMut(&key);
                    //io::print(fmt!("Bestkey: %s,%s\n",key,bestkey));
                    if(*bestkey == key) { return Some(util::replace(val,value));}
                    match(key.mismatch(bestkey)) {
                        Some((count,b1,b2)) => {
                            let mut x = b1^b2;
                            //io::print(format!("not found {} : {} : {} : {:x} : {:x}\n",*bestkey,key,count,b1,b2));
                            while (x & (x - 1) != 0) {
                                x &= (x - 1);
                            }
                            (count,x)
                        },
                        None => fail!(),
                    }
                };
                self.size+=1;
                node.addnew(key,value,len,mask);
            },
            None => self.root = Some(externalNode(~ExternalNode {key: key, value : value}))
        }
        return None;
    }

    fn del(&mut self, key : &K) -> Option<V> {
        let mut root = self.root.take();
        let mut retval = None;
        match root {
            Some(ref mut x) => {
                match x {
                    &externalNode(ref mut n) => {
                        if n.key == *key {
                            let mut tmp = ~ExternalNode{key : Default::default(), value : Default::default()};
                            swap(n,&mut tmp);
                            return Some(tmp.value)
                        }
                    },
                    &internalNode(_) => {
                        retval = x.del(key)
                    }
                }
            },
            _ => ()
        }
        self.root=root;
        return retval;
    }

    fn get<'a>(&'a self, key : &K) -> Option<&'a V> {
        match self.root {
            Some(ref node) => {
                let (bk,bv) = node.findbest(key);
                if(bk == key) {
                    Some(bv)
                }
                else {
                    None
                }
            },
            _ => None
        }
    }
}

impl<K : fmt::Default + Eq + Bitable + Default,V : Default + fmt::Default> Map<K,V> for CritbitTree<K,V> {
    fn find<'a>(&'a self, key : &K) -> Option<&'a V> {
        self.get(key)
    }
    fn contains_key(&self, key : &K) -> bool {
        self.get(key).is_some()
    }
}

impl<K : fmt::Default + Eq + Bitable + Default,V : Default + fmt::Default> MutableMap<K,V> for CritbitTree<K,V> {
    fn swap(&mut self, k : K, v : V) -> Option<V> {
        self.set(k,v)
    }
    fn pop(&mut self, k : &K) -> Option<V> {
        self.del(k)
    }
    fn find_mut<'a>(&'a mut self, k : &K) -> Option<&'a mut V> {
        match self.root {
            Some(ref mut n) => {
                let (oldk,oldv) = n.findbestMut(k);
                if k == oldk {
                    Some(oldv)
                }
                else {
                    None
                }
            },
            _ => None,
        }
    }
}


   fn printTree<K : Bitable + Eq + fmt::Default + Default,V : fmt::Default + Default> (c : &CritbitTree<K,V>) {
       match c.root {
           Some(ref x) => 
               printTreeInternal(x,0),
               None => io::print("(Empty)\n")
       }
   }

   fn spaces(n : uint) {
   let mut i = n;
   while (i > 0) {
   io::print(" ");
   i-=1;
   }
   }

fn printTreeInternal<K : Bitable + Eq + fmt::Default + Default,V : fmt::Default + Default>(n : &Node<K,V>, indent : uint) {
    spaces(indent);
    match n {
        &internalNode(ref n) => {
            io::print(format!("Internal {:x}\n",n.len));
            printTreeInternal(&n.left,indent+1);
            printTreeInternal(&n.right,indent+1);
        },
        &externalNode(ref n) => {
            io::print(format!("External {} : {}\n",n.key,n.value))
        }
    }
}

fn test()
{
    let mut tree : ~CritbitTree<~str,~str> = ~CritbitTree {root : None, size : 0};
    assert!(tree.del(&~"test")==None);
    assert!(tree.get(&~"test")==None);
    tree.set(~"test",~"value");
    assert!(tree.get(&~"test") == Some(&~"value"));
    assert!(tree.get(&~"value2")==None);
    tree.set(~"test2",~"value2");
    assert!(tree.get(&~"test2") == Some(&~"value2"));
    assert!(tree.get(&~"test") == Some(&~"value"));
    tree.set(~"tes33",~"value3");
    assert!(tree.get(&~"tes33") == Some(&~"value3"));
    assert!(tree.get(&~"test2") == Some(&~"value2"));
    assert!(tree.get(&~"test") == Some(&~"value"));
    tree.set(~"aes33",~"value3");
    assert!(tree.get(&~"aes33") == Some(&~"value3"));
    assert!(tree.get(&~"tes33") == Some(&~"value3"));
    assert!(tree.get(&~"test2") == Some(&~"value2"));
    assert!(tree.get(&~"test") == Some(&~"value"));
    tree.set(~"test",~"value2");
    assert!(tree.get(&~"aes33") == Some(&~"value3"));
    assert!(tree.get(&~"tes33") == Some(&~"value3"));
    assert!(tree.get(&~"test2") == Some(&~"value2"));
    assert!(tree.get(&~"test") == Some(&~"value2"));
    assert!(tree.del(&~"aes33") == Some(~"value3"));
    assert!(tree.del(&~"aes33") == None);
    assert!(tree.del(&~"test2") == Some(~"value2"));
    assert!(tree.del(&~"test") == Some(~"value2"));
}


fn benchmarkMap<M : MutableMap<~str,~str>>(map : &mut M, count : uint,mask : uint) {
    let mut i = 0;
    while(i<count) {
        map.insert(format!("{:u},",mask&rand::random::<uint>()),
        format!("{:u},",rand::random::<uint>()));
        i+=1;
    }
}


fn benchmarkMapInt<M : MutableMap<uint,uint>>(map : &mut M, count : uint,mask : uint) {
    let mut i = 0;
    while(i<count) {
        map.insert(mask&rand::random::<uint>(), rand::random::<uint>());
        i+=1;
    }
}

fn benchmarkCb(count : uint,mask : uint) {
    let mut tree : CritbitTree<~str,~str> = CritbitTree {root : None, size : 0};
    benchmarkMap(&mut tree,count,mask);
    tree.clear();
}

fn benchmarkCbInt(count : uint,mask : uint) {
    let mut tree : CritbitTree<uint,uint> = CritbitTree {root : None, size : 0};
    benchmarkMapInt(&mut tree,count,mask);
    tree.clear();
}

fn benchmarkBare(count : uint,mask : uint) {
    let mut i = 0;
    while(i<count) {
        format!("{:u},",mask&rand::random::<uint>());
        format!("{:u},",rand::random::<uint>());
        i+=1;
    }
}

fn benchmarkHashInt(count : uint,mask : uint) {
    let mut hash : hashmap::HashMap<uint,uint> = hashmap::HashMap::new();
    benchmarkMapInt(&mut hash,count,mask)
}

fn benchmarkHash(count : uint,mask : uint) {
    let mut hash : hashmap::HashMap<~str,~str> = hashmap::HashMap::new();
    benchmarkMap(&mut hash,count,mask)
}


fn main() {
    let args : ~[~str] = os::args();
    let countO : Option<uint> = num::from_str_radix(args[1],10);
    let mut mask = uint::MAX;
    if(args.len() > 3) {
        match num::from_str_radix(args[3],16) {
            Some(m) => mask=m,
            _ => ()
        }
    }
    match countO {
        Some(count) => {
            test();
            match args[2] {
                ~"critbit" => benchmarkCb(count,mask),
                ~"icritbit" => benchmarkCbInt(count,mask),
                ~"bare" => benchmarkBare(count,mask),
                ~"hash" => benchmarkHash(count,mask),
                ~"ihash" => benchmarkHashInt(count,mask),
                _ => fail!()
            }
        },
        None() => fail!()
    }
}
