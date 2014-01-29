use std::util::swap;
use std::io;
use std::rand;
use std::os;
use std::uint;
use std::hashmap;
use std::num;
use std::util;

Trait Bitable {
	//Get the Nth logical byte
	fn getb(&self, off: uint) -> u8;
}

/*
Trait Zero {
	fn zero() -> Self;
}

impl Bitable for ~str {
	fn getb(&self, off: uint) {
		let bytes = s.as_bytes();
		if(off  >= bytes.len()) {
			0x00
		}
		else {
			//str::as_bytes(s) {|bytes| bytes[off]}
			bytes[off]
		}
	}
}

impl Zero for ~str {
	fn zero() -> ~str {
		~""
	}
}
*/


struct ExternalNode {
    key: ~str,
    value: ~str
}

struct InternalNode {
    left: Node,
    right: Node,
    len: uint
}

struct CritbitTree {
    root: Option<Node>,
    size: uint,
}

enum Node {
    internalNode(~InternalNode),
    externalNode(~ExternalNode)
}

impl InternalNode {
    fn pop_left(&mut self ,replace : Node)  -> Node {
        util::replace(&mut self.left,replace)
    }
    fn pop_right(&mut self, replace : Node) -> Node {
        util::replace(&mut self.right,replace)
        }
}

pub struct NodeIterator {
    priv cur: Option<Node>,
}

impl Iterator<uint> for NodeIterator {
    fn next(&mut self) -> Option<uint> {
        match self.cur.take() {
            Some(externalNode(_)) => None,
            None => None,
            Some(internalNode(cur)) => {
                let mut cur = cur;
                loop {
                    let dummy = externalNode(~ExternalNode {key: ~"",
                                              value :~""});
                    match cur.pop_left(dummy) {
                        internalNode(node) => {
                            let mut node = node;
                            let dummy = externalNode(~ExternalNode {key : ~"",
                                              value :~""});
                            cur.left = node.pop_right(dummy);
                            node.right = internalNode(cur);
                            cur = node;
                        }

                        externalNode(n) => {
                            self.cur = Some(cur.pop_right(externalNode(n)));
                            // left and right fields are both None
                            return Some(1);
                        }
                    }
                }
            },
        }
    }

}

impl Container for CritbitTree {
    fn len(&self) -> uint {self.size}
    fn is_empty(&self) -> bool {self.len() == 0}
}

impl Mutable for CritbitTree {
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

fn getb(s: &str, off: uint) -> u8{
    let bytes = s.as_bytes();
    if(off  >= bytes.len()) {
        0x00
    }
    else {
        //str::as_bytes(s) {|bytes| bytes[off]}
        bytes[off]
    }
}

fn mismatch (s1:&str,s2:&str) -> Option<(uint,u8,u8)> {
    let s1b = s1.as_bytes();
    let s2b = s2.as_bytes();
    let mut it = s1b.iter().zip(s2b.iter()).enumerate();
    loop
    {
        match it.next() {
            Some((count,(c1,c2))) => if(c1 != c2) {
            //io::print(fmt!("Mismatch: %u,%u,%u\n",count,*c1 as uint, *c2 as uint));
            return Some((count,*c1,*c2)) },
            None() =>
                return
                if(s1.len() > s2.len()) { Some((s2.len(),s1[s2.len()],0))}
                else if(s2.len() > s1.len()) { Some((s1.len(),0,s2[s1.len()]))}
                else { None }
        }
    }
}

fn goleft (key: &str, off: uint, mask: u8) -> bool{
  //io::print(fmt!("goleft %s,%u,%ux->%ux\n",key,off,mask as uint,(getb(key, off) & mask) as uint));
  getb(key, off) & mask == 0
}


fn findbest(mut cb: &Node, key :&str) -> (~str,~str) {
    loop {
        let tmp = cb;
        match tmp {
            &internalNode(ref n) =>{
                //{left : left, right : right, length : length})=> {
                if (goleft(key,lenToOffset(n.len),lenToMask(n.len))) {
                    cb = &n.left;
                }
                else {
                    cb = &n.right;
                }
            }
            &externalNode(ref n) => {
                return (n.key.clone(),n.value.clone())
            }
            }
    }
}
//TODO convert to loop
fn findbestR(cb: &~Node, key :&str) -> (~str,~str) {
    match cb {
        &~internalNode(ref n) =>{
        //{left : left, right : right, length : length})=> {
            if (goleft(key,lenToOffset(n.len),lenToMask(n.len))) {
                findbest(&n.left,key)
            }
            else {
                findbest(&n.right,key)
            }
        }
        &~externalNode(ref n) => {
            (n.key.clone(),n.value.clone())
        }
    }
}

fn replace(mut cb: &mut Node, key :&str, value : ~str) {
    loop {
        let tmp = cb;
    match tmp {
        &internalNode(ref mut n) =>{
            //{left : left, right : right, length : length})=> {
            if (goleft(key,lenToOffset(n.len),lenToMask(n.len))) {
                cb = &mut n.left
                //replace(&mut n.left,key,value)
            }
            else {
                //replace(&mut n.right,key,value)
                cb = &mut n.right;
            }
        }
        &externalNode(ref mut n) => {
            n.value = value;
            return;
        }
    }
    }
}

fn replaceR(cb: &mut ~Node, key :&str, value : ~str) {
    match cb {
        &~internalNode(ref mut n) =>{
            //{left : left, right : right, length : length})=> {
            if (goleft(key,lenToOffset(n.len),lenToMask(n.len))) {
                replace(&mut n.left,key,value)
            }
            else {
                replace(&mut n.right,key,value)
            }
        }
        &~externalNode(ref mut n) => {
            n.value = value
        }
    }
}

fn insertAtPoint<'a>(cb : &'a mut Node, key : ~str, value : ~str,
                     count: uint, mask :u8)
{
    let direction = goleft(key,count,mask);
    let newExtNode = externalNode(~ExternalNode { key: key, value : value });
    let mut tmp = externalNode(~ExternalNode { key : ~"", value : ~""});
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

fn findInsertionPoint<'a>(cb : &mut Node, key : ~str, value: ~str, bestMatch: &str,
        count : uint, mask : u8)
{
        match *cb {
            externalNode(_) => 
            insertAtPoint(cb,key,value,count,mask),
            
            internalNode(~InternalNode { left : _, right : _, len: len}) 
                if(lenToOffset(len) > count ||
                (lenToOffset(len) == count &&
                        lenToMask(len) < mask))
            => 
                    insertAtPoint(cb,key,value,count,mask),
            internalNode(~InternalNode{ left : _, right : _, len: len}) =>
                {
                    let direction = goleft(key,lenToOffset(len),lenToMask(len));
                    findInsertionPointInternal(cb,key,value,bestMatch,count,mask,direction)
                }
        }
}

fn isInsertionPoint<'n>(cb: &Node, count : uint, direction : bool, mask : u8) -> bool {
    match cb{
        &externalNode(_) => fail!(),
        &internalNode(ref cb) =>
            match if (direction) { &'n cb.left } else {&'n cb.right} {
                &externalNode(_) => true,
                &internalNode(ref n)
                    if(lenToOffset(n.len) > count) ||
                        (lenToOffset(n.len) == count &&
                         lenToMask(n.len) < mask) => true,
                            _ => false
            }
    }
}

fn findInsertionPointInternal(mut cb : &mut Node, key : ~str,
        value: ~str, bestMatch: &str,
        count : uint, mask : u8, mut direction : bool)
{
    loop {
        let tmp = cb;
        if(isInsertionPoint(tmp,count,direction,mask)) {
            return insertAtPoint(tmp,key,value,count,mask)
        }
        else {
            match tmp {
                &externalNode(_) => {
                    fail!();
                }
                &internalNode(ref mut cbi) =>
                {
                    let child = if (direction) { &mut cbi.left }
                    else { &mut cbi.right };
                    match child {
                        &internalNode(~InternalNode { left: _, right: _, len: len}) => {
                            let newDirection = goleft(key,lenToOffset(len),lenToMask(len));
                            cb = child;
                            direction = newDirection;
                        }
                        _ => fail!()
                    }
                }
            }
        }
    }
}


fn addnew(cb : &mut Node,key : ~str,value : ~str,bestMatch : ~str) {
/*
    io::print("addnew ");
    io::print(key);
    io::print(" ");
    io::print(bestMatch);
    io::print("\n");
    */
    match(mismatch(key,bestMatch)) {
        Some((count,b1,b2)) => {
            let mut x = b1^b2;
            while (x & (x - 1) != 0) {
                x &= (x - 1);
            }
            findInsertionPoint(cb,key,value,bestMatch,count,x)
        },
    None => fail!()
    }
}

fn cbset(cb: &mut~CritbitTree, key : ~str, value : ~str)
{
    match cb.root {
        Some(ref mut node) =>
        {
            let (bestkey,_) = findbest(node, key);
            //io::print(fmt!("Bestkey: %s,%s\n",key,bestkey));
            if(bestkey == key) { replace(node,key,value) } //TODO handle this 
            else {
                cb.size+=1;
                addnew(node,key,value,bestkey)
            }
        },
            None => cb.root = Some(externalNode(~ExternalNode {key: key, value : value}))
    }
}

fn cbsetR(cb: &mut~CritbitTree, key : ~str, value : ~str)
{
    match cb.root {
        Some(ref mut node) =>
        {
            let (bestkey,_) = findbest(node, key);
            //io::print(fmt!("Bestkey: %s,%s\n",key,bestkey));
            if(bestkey == key) { replace(node,key,value) } //TODO handle this 
            else {
                addnew(node,key,value,bestkey)
            }
        },
            None => cb.root = Some(externalNode(~ExternalNode {key: key, value : value}))
    }
}

fn cbget(cb: &~CritbitTree, key : &str) -> Option<~str> {
    match cb.root {
        Some(ref node) => {
            let (bk,bv) = findbest(node,key.to_str());
            if(bk.equiv(&key)) {
                Some(bv.clone())
            }
            else {
                None
            }
        },
        _ => None
    }
}

fn printTree (c : &~CritbitTree) {
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
fn printTreeInternal(n : &Node, indent : uint) {
    spaces(indent);
    match n {
        &internalNode(ref n) => {
            io::print(format!("Internal {:x}\n",n.len));
            printTreeInternal(&n.left,indent+1);
            printTreeInternal(&n.right,indent+1);
        },
        &externalNode(ref n) => {
            io::print("External ");
            io::print(n.key);
            io::print(":");
            io::print(n.value);
            io::print("\n");
        }
    }
}

fn test()
{
    let mut tree = ~CritbitTree {root : None, size : 0};
    assert!(cbget(&tree,&"test")==None);
    cbset(&mut tree,~"test",~"value");
    assert!(cbget(&tree,&"test") == Some(~"value"));
    cbset(&mut tree,~"test2",~"value2");
    assert!(cbget(&tree,&"test2") == Some(~"value2"));
    assert!(cbget(&tree,&"test") == Some(~"value"));
    cbset(&mut tree,~"tes33",~"value3");
    assert!(cbget(&tree,&"tes33") == Some(~"value3"));
    assert!(cbget(&tree,&"test2") == Some(~"value2"));
    assert!(cbget(&tree,&"test") == Some(~"value"));
    cbset(&mut tree,~"aes33",~"value3");
    assert!(cbget(&tree,&"aes33") == Some(~"value3"));
    assert!(cbget(&tree,&"tes33") == Some(~"value3"));
    assert!(cbget(&tree,&"test2") == Some(~"value2"));
    assert!(cbget(&tree,&"test") == Some(~"value"));
    cbset(&mut tree,~"test",~"value2");
    assert!(cbget(&tree,&"aes33") == Some(~"value3"));
    assert!(cbget(&tree,&"tes33") == Some(~"value3"));
    assert!(cbget(&tree,&"test2") == Some(~"value2"));
    assert!(cbget(&tree,&"test") == Some(~"value2"));
}

fn benchmarkCb(count : uint) {
    let mut tree = ~CritbitTree {root : None, size : 0};
    let mut i = 0;
    while(i<count) {
        cbset(&mut tree, format!("{:u},",rand::random::<uint>()),
                format!("{:u},",rand::random::<uint>()));
        i+=1;
    }
    tree.clear();
}

fn benchmarkBare(count : uint) {
    let mut i = 0;
    while(i<count) {
        format!("{:u},",rand::random::<uint>());
        format!("{:u},",rand::random::<uint>());
        i+=1;
    }
}

fn benchmarkHash(count : uint) {
    let mut hash : hashmap::HashMap<~str,~str> = hashmap::HashMap::new();
    let mut i = 0;
    while(i<count) {
        hash.insert(format!("{:u},",rand::random::<uint>()),
                format!("{:u},",rand::random::<uint>()));
        i+=1;
    }
}

fn main() {
    let args : ~[~str] = os::args();
    let countO : Option<uint> = num::from_str_radix(args[1],10);
    match countO {
        Some(count) => {
            test();
            match args[2] {
                ~"critbit" => benchmarkCb(count),
                    ~"bare" => benchmarkBare(count),
                    ~"hash" => benchmarkHash(count),
                    _ => fail!()
            }
        },
        None() => fail!()
    }
}
