use std::collections::HashMap;

use serde::{Deserialize, Serialize};

#[derive(Debug, Default, Clone)]
struct KeyValueStore {
    store: HashMap<Vec<u8>, Vec<u8>>,
}

impl KeyValueStore {
    fn put(&mut self, key: &[u8], value: &[u8]) {
        self.store.insert(key.to_vec(), value.to_vec());
    }

    fn get(&self, key: &[u8]) -> Option<Vec<u8>> {
        self.store.get(key).cloned()
    }
}

type Value = u64;
type Hash = u64;
type NodeId = u64;

#[derive(Debug, Clone, Serialize, Deserialize)]
enum NodeType {
    // ref count, value
    LeafValue(u64, Value),
    /// ref count, hash, left, right
    IntermediateValue(u64, Hash, NodeId, NodeId),
}

fn value_hash(v: &Value) -> Hash {
    *v
}

fn concat_hashes(h1: &Hash, h2: &Hash) -> Hash {
    *h1 + *h2
}

impl NodeType {
    fn get_children(&self) -> (NodeId, NodeId) {
        match self {
            NodeType::IntermediateValue(_, _, left, right) => (*left, *right),
            NodeType::LeafValue(..) => panic!("leaf value does not have children"),
        }
    }

    fn get_hash(&self) -> Hash {
        match self {
            NodeType::LeafValue(_, value) => value_hash(value),
            NodeType::IntermediateValue(_, hash, _, _) => *hash,
        }
    }

    fn inc_ref_count(&mut self) -> u64 {
        let rc = match self {
            NodeType::LeafValue(rc, _) => rc,
            NodeType::IntermediateValue(rc, _, _, _) => rc,
        };
        *rc += 1;
        *rc
    }

    // fn dec_ref_count(&mut self) -> u64 {
    //     let rc = match self {
    //         NodeType::LeafValue(rc, _) => rc,
    //         NodeType::IntermediateValue(rc, _, _, _) => rc,
    //     };
    //     *rc += 1;
    //     *rc
    // }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct Node {
    node_id: NodeId,
    node_type: NodeType,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct MerkleTreeHeader {
    tree_id: u64,
    depth: u64,
    last_used_node_id: u64,
    root_node: NodeId,
}

impl MerkleTreeHeader {
    pub fn get_node_key(tree_id: u64, node_id: NodeId) -> Vec<u8> {
        [
            &b"tree"[..],
            &tree_id.to_be_bytes(),
            &b"node"[..],
            &node_id.to_be_bytes(),
        ]
        .concat()
    }
}

#[derive(Debug)]
struct MerkleTree<'a> {
    header: MerkleTreeHeader,
    kv_store: &'a mut KeyValueStore,
}

impl<'a> MerkleTree<'a> {
    // fn new(header: MerkleTreeHeader, kv_store: &'a mut KeyValueStore) -> Self {
    //     Self {
    //         header,
    //         kv_store
    //     }
    // }

    fn insert_node_storage(tree_id: u64, node: &Node, kv_store: &mut KeyValueStore) {
        let key = MerkleTreeHeader::get_node_key(tree_id, node.node_id);
        let value = serde_json::to_vec(node).expect("node ser");
        kv_store.put(&key, &value);
    }

    fn get_node_storage(tree_id: u64, node_id: NodeId, kv_store: &KeyValueStore) -> Option<Node> {
        let key = MerkleTreeHeader::get_node_key(tree_id, node_id);
        kv_store
            .get(&key)
            .map(|v| serde_json::from_slice(&v).expect("node deser"))
    }

    pub fn create_default_tree(
        tree_id: u64,
        default_value: Value,
        depth: u64,
        kv_store: &'a mut KeyValueStore,
    ) -> Self {
        assert!(depth > 1);
        let mut last_used_node_id = 0;

        let leaf_node = Node {
            node_id: last_used_node_id,
            node_type: NodeType::LeafValue(0, default_value),
        };
        last_used_node_id += 1;
        Self::insert_node_storage(tree_id, &leaf_node, kv_store);

        // start to walk tree bottom-up
        for d in 1..=depth {
            let mut prev_node_level =
                Self::get_node_storage(tree_id, d - 1, kv_store).expect("bug in algo, walking up");

            let prev_hash = prev_node_level.node_type.get_hash();
            let current_hash = concat_hashes(&prev_hash, &prev_hash);

            assert_eq!(last_used_node_id, d);
            let new_node = Node {
                node_id: last_used_node_id,
                node_type: NodeType::IntermediateValue(
                    0,
                    current_hash,
                    prev_node_level.node_id,
                    prev_node_level.node_id,
                ),
            };
            last_used_node_id += 1;
            Self::insert_node_storage(tree_id, &new_node, kv_store);

            prev_node_level.node_type.inc_ref_count();
            prev_node_level.node_type.inc_ref_count();
            Self::insert_node_storage(tree_id, &prev_node_level, kv_store);
        }

        let mut root_node =
            Self::get_node_storage(tree_id, depth, kv_store).expect("failed to get root node");
        root_node.node_type.inc_ref_count();
        Self::insert_node_storage(tree_id, &root_node, kv_store);

        let header = MerkleTreeHeader {
            tree_id,
            depth,
            last_used_node_id,
            root_node: root_node.node_id,
        };

        MerkleTree { header, kv_store }
    }

    pub fn get_path(&self, idx: u64) -> (Value, Vec<Hash>) {
        assert!(idx < 2u64.pow(self.header.depth as u32));

        let mut path = Vec::new();
        for d in 0..self.header.depth {
            path.push(idx & (1 << d) != 0)
        }
        path.reverse();

        let mut hashes = Vec::new();

        let mut next_node =
            Self::get_node_storage(self.header.tree_id, self.header.root_node, self.kv_store)
                .expect("root node not found");
        for go_right in path {
            let (left, right) = next_node.node_type.get_children();
            let (next_node_id, neighbour_node_id) = if go_right {
                (right, left)
            } else {
                (left, right)
            };

            let neighbour_node =
                Self::get_node_storage(self.header.tree_id, neighbour_node_id, self.kv_store)
                    .expect("neighbour node not found");
            hashes.push(neighbour_node.node_type.get_hash());

            next_node = Self::get_node_storage(self.header.tree_id, next_node_id, self.kv_store)
                .expect("next node not found");
        }
        hashes.reverse();

        let value = match next_node.node_type {
            NodeType::LeafValue(_, value) => value,
            NodeType::IntermediateValue(..) => panic!("should have leaf node at the bottom"),
        };

        (value, hashes)
    }

    pub fn check_path(&self, idx: u64, value: &Value, hashes: &[Hash]) -> bool {
        assert!(idx < 2u64.pow(self.header.depth as u32));

        let mut path = Vec::new();
        for d in 0..self.header.depth {
            path.push(idx & (1 << d) != 0);
        }

        let mut current_hash = value_hash(&value);
        for (is_this_right_node, proof_hash) in path.into_iter().zip(hashes) {
            current_hash = if is_this_right_node {
                concat_hashes(proof_hash, &current_hash)
            } else {
                concat_hashes(&current_hash, proof_hash)
            };
        }

        let root_node =
            Self::get_node_storage(self.header.tree_id, self.header.root_node, self.kv_store)
                .expect("next node not found");

        root_node.node_type.get_hash() == current_hash
    }
}

fn main() {
    let mut kv = KeyValueStore::default();

    let tree = MerkleTree::create_default_tree(0, 11, 3, &mut kv);
    let (v, hashes) = tree.get_path(3);

    dbg!(&v);
    dbg!(&hashes);

    dbg!(tree.check_path(3, &v, &hashes));

    let mut incorrect_hashes = hashes;
    incorrect_hashes[2] = incorrect_hashes[1];

    dbg!(tree.check_path(3, &v, &incorrect_hashes));
}
