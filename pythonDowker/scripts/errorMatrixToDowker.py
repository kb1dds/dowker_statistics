#!/usr/bin/env python
# -*- coding: utf-8 -*-
########################################
## dowker
########################################
# Description
########################################
# Copyright (C) 2022 BAE Systems.
# See LICENSE for full copyright and licensing.
########################################
import csv
import json
import numpy
import matplotlib.pyplot as plt
import copy
import networkx as nx
import scipy
import sys
import random
import math
import igraph as ig

from itertools import chain, combinations
import itertools

from plotly.offline import iplot

import plotly.graph_objs as go
import plotly

import time

#For limiting a dowker size for readability to check how the layout looks
MAXLEN=10008
pg_cache = {}


##Global dialect detection variables
node_dialects_map = dict()
node_weight_left_map = dict()
node_dialect_weight_map = dict()
visited_this_iteration = list()
dialect_messages_map = dict()
dialect_files_map = dict()

bad_edges = 0

roots = set()

###HELPER FUNCTIONS FOR PROCESSING ERROR MATRIX ###

#Turns binary row into readable feature string, ie [0,1,0, 0, 1] -> 1,4

def process_node_cached(node, row_hash_str):
    if row_hash_str in pg_cache:
        return pg_cache[row_hash_str]
    else:
        name=""
        for index in range(len(node)):
            if (node[index]==1):
                name=name+str(index)+","

        name=name.rstrip(",")

        return name

#Gets hash of binary row
def binary_row_hash( row ):
    return numpy.packbits(row.astype(numpy.bool_)).tobytes().hex()
def get_sublist(xs):
    return [list(subset) for i in range(0, len(xs))
                  for subset in itertools.combinations(xs, i)]
##    res = [ls[x:y] for x, y in combinations( range(len(ls) + 1), r = 2)]
##    res.append([])
##    return res
##    
##    lists = list()
##    chain_obj = chain.from_iterable(combinations(ls, r) for r in range(1,len(ls)+1))
##    for i in chain_obj:
##        lists.append(i)

    return lists

## Node positioning helper functions

def collect_neighbors_layer_layout(length_node_map, node_edge_map):
    string_pos_map = dict()
    keys  = list(length_node_map.keys())
    keys.sort()
    for length in keys:

        if length> MAXLEN:
            break
        if length==0:
            length_node_map[length].sort()
            n = len(length_node_map[length])
            k=0
            for display_name in length_node_map[length]:
                tk= 2.0*math.pi*k/n
                r=math.sqrt(n)/2
                string_pos_map[display_name]=(r*math.cos(tk), r*math.sin(tk), length)
                k+=1
        else:
            anchorNodesMap = dict()
            anchorNodesMap[(length, length, length)]=list()
            for display_name in length_node_map[length]:
                matched=False
                if display_name in node_edge_map:
                    for node in node_edge_map[display_name]:
                        if node in string_pos_map:
                            if not(string_pos_map[node]) in anchorNodesMap:
                                anchorNodesMap[string_pos_map[node]]=list()
                            anchorNodesMap[string_pos_map[node]].append(display_name)
                            matched=True
                            
                if not(matched):
                    anchorNodesMap[(length, length,length)].append(display_name)
            for anchor in anchorNodesMap:
                
                k=0
                n = len(anchorNodesMap[anchor])
                if anchor == (length, length, length):
                    for display_name in anchorNodesMap[anchor]:
                        x = random.uniform(0, 5)
                        y = random.uniform(0, 5)
                        string_pos_map[display_name]=(x, y, length)
                           
                else:
                    if n==1:
                       
                        string_pos_map[anchorNodesMap[anchor][0]]=(anchor[0], anchor[1], length)
                     
                    else:
                    
                        for display_name in anchorNodesMap[anchor]:
                            tk= 2.0*math.pi*k/n
                            r=math.sqrt(n)/2
                            string_pos_map[display_name]=(anchor[0] +  r*math.cos(tk), anchor[1] + r*math.sin(tk), length)
                            k+=1

    return string_pos_map




def original_circular_layer_layout(length_node_map):
    string_pos_map = dict()
    for length in length_node_map:
        length_node_map[length].sort()
        n = len(length_node_map[length])
        k=0
        for display_name in length_node_map[length]:
            tk= 2.0*math.pi*k/n
            r=math.sqrt(n)
            string_pos_map[display_name]=(r*math.cos(tk), r*math.sin(tk), length)
            k+=1
    return string_pos_map
    
def layer_2_spring_layout(length_node_map, node_edge_map):
    string_pos_map = dict()
    maxLen = max(list(length_node_map.keys()))
    maxLen = min(maxLen, MAXLEN)
 
    for length in range(0, maxLen, 2):
        G = nx.Graph()
        #Add all nodes in layer1
        if length in length_node_map:
            for node in length_node_map[length]:
                G.add_node(node)
        #Add all nodes in layer2
        if (length+1) in length_node_map:
            for node in length_node_map[length+1]:
                G.add_node(node)
        #Add all edges to graph
        if length in length_node_map:   
            for node in length_node_map[length]:
                for conn_node in node_edge_map[node]:
                    G.add_edge(node, conn_node)
        #Try nx force-directed graph
        pos = nx.spring_layout(G)
        if length in length_node_map:
            for display_name in length_node_map[length]:
                string_pos_map[display_name]=(pos[display_name][0], pos[display_name][1], length+1)
        if (length+1) in length_node_map:
            for display_name in length_node_map[length+1]:
                string_pos_map[display_name]=(pos[display_name][0], pos[display_name][1], length+1)

    return string_pos_map    

def full_force_directed_layers(all_nodes, node_edge_map):
    string_pos_map = dict()
    #Build a ig graph
    G = ig.Graph()
    #Add nodes to ig graph
    for node in all_nodes:
        G.add_vertex(str(node))
    #Add edges to ig graph
    for node in node_edge_map:
        for conn_node in node_edge_map[node]:
            if conn_node in all_nodes and node in all_nodes:
                G.add_edge(str(node), str(conn_node))
    #kk for Kamada-Kawai, fr for Fruchterman-Reingold
    layt=G.layout('fr', dim=3)
    i=0
    for node in all_nodes:
        z = len(node.split(","))+1
#        print(layt[i])
        string_pos_map[node]=(layt[i][0], layt[i][1], z)
        i+=1
  
    return string_pos_map
def full_force_directed(all_nodes, node_edge_map):
    string_pos_map = dict()
    #Build a ig graph
    G = ig.Graph()
    #Add all nodes to ig graph
    for node in all_nodes:
        G.add_vertex(str(node))
    #Add edges to ig graph
    for node in node_edge_map:
        for conn_node in node_edge_map[node]:
            if conn_node in all_nodes and node in all_nodes:
                G.add_edge(str(node), str(conn_node))
    
    layt=G.layout('kk', dim=3)
    i=0
    for node in all_nodes:
        z = len(node.split(","))+1
#        print(layt[i])
        string_pos_map[node]=(layt[i][0], layt[i][1], layt[i][2])
        i+=1
  
    return string_pos_map

### Inconsistent edge coloring ###
def color_by_consistent_edge(node1, node2, node_count_map):
    global bad_edges
    high_count = node_count_map[node1] 
    low_count = node_count_map[node2]
    if high_count > low_count:
        bad_edges+=1
        return 'red'
    else:
        return 'green'

### Node coloring options ###

def color_by_weight(node, node_count_map):
    return math.log(node_count_map[node])

def color_by_dialect(node, node_feature_indices_map):
    global node_dialects_map
    display_name = node_feature_indices_map[node]
    if display_name in node_dialects_map:
        for i in range(160, -1, -1):
            dialect = "dialect_" + str(i)
            if dialect in node_dialects_map[display_name]:
                return i
    else:
        return 0
def color_by_dialect_number(node, node_feature_indices_map):
    global node_dialects_map
    display_name = node_feature_indices_map[node]
    if display_name in node_dialects_map:
        return len(node_dialects_map[display_name])
    else:
        return 0
################################################
#### FUNCTIONS FOR DOWKER APPEARANCE ###########
################################################
def create_string_pos_map(all_nodes, length_node_map, node_edge_map, layout_scheme):
    # Select layout by layout_scheme
    if layout_scheme==0:
        #Default layout: Original circular layout places nodes of the same length in a circle
        string_pos_map = original_circular_layer_layout(length_node_map)
    
    elif layout_scheme==1:
        #Group together connected nodes
        string_pos_map =  collect_neighbors_layer_layout(length_node_map, node_edge_map)
    elif layout_scheme==2:
        #Use 2D-spring layout for each pair of layers, while separating layers by setting z coordinate to length
        string_pos_map = layer_2_spring_layout(length_node_map, node_edge_map)
    elif layout_scheme==3:
        #Graph all nodes together with a KK (or fr) layout
        string_pos_map = full_force_directed(all_nodes, node_edge_map)
    elif layout_scheme ==4:
        #Graph all nodes together, but separate layers by length as z value
        string_pos_map = full_force_directed_layers(all_nodes, node_edge_map)

    return string_pos_map


def size_by_weight(node, node_count_map):
    # Min size node as 7, max size as 20 (for best display)
    if node in node_count_map:
        val = node_count_map[node]
        if val< 7:
            return 7
        if val > 20:
            return 20
        return val

    return 5

def get_node_size(node, displayName, node_count_map, node_size_scheme):
    #Select node size scheme
    if node_size_scheme==0:
        #default as all size 10
        return 10
    elif node_size_scheme==1:
        return size_by_weight(node, node_count_map)

def get_edge_color(node1, node2, node_count_map, edge_color_scheme):
    #Select which edge coloring technique to use
    if edge_color_scheme==0:
        return 'black'
    elif edge_color_scheme ==1:
        #Inconsistent edges in red, consistent edges in green
        return color_by_consistent_edge(node1, node2, node_count_map)

def get_display_name(node, node_feature_indices_map):
    #Name that shows up in hovertext
    #Using the string of feature indices is implemented
    return node_feature_indices_map[node]

def color_by_dialect_gradient_hex(node, node_feature_indices_map):
    global node_dialect_weight_map
    index0=0
    index1=0
    index2=0
    index3=0
    index4=0
    index5=0
    display_name = node_feature_indices_map[node]
    if display_name in node_dialect_weight_map:
        weight_list = node_dialect_weight_map[display_name]
        for dialect_weight in weight_list:
            if 'dialect_0' in dialect_weight:
                index0= int(dialect_weight.split("-")[1])
            if 'dialect_1' in dialect_weight:
                index1= int(dialect_weight.split("-")[1])
            if 'dialect_2' in dialect_weight:
                index2= int(dialect_weight.split("-")[1])
            if 'dialect_3' in dialect_weight:
                index3= int(dialect_weight.split("-")[1])
            if 'dialect_4' in dialect_weight:
                index4= int(dialect_weight.split("-")[1])                
            if 'dialect_5' in dialect_weight:
                index5= int(dialect_weight.split("-")[1])                    

        index0 = 9*index0/(index0+index1+index2+index3+index4+index5)
        index1 = 9*index1/(index0+index1+index2+index3+index4+index5)
        index2 = 9*index2/(index0+index1+index2+index3+index4+index5)
        index3 = 9*index3/(index0+index1+index2+index3+index4+index5)
        index4 = 9*index4/(index0+index1+index2+index3+index4+index5)
        index5 = 9*index5/(index0+index1+index2+index3+index4+index5)
      
        index0=int(min(index0,9))
        index1=int(min(index1,9))
        index2=int(min(index2,9))
        index3=int(min(index3,9))
        index4=int(min(index4,9))
        index5=int(min(index5,9))

        val = "#" + str(index0) + str(index1) + str(index2) + str(index3) + str(index4) + str(index5) 
        
        return val
    else:
        return '#000000'    

def color_by_dialect_gradient(node, node_feature_indices_map):
    global node_dialect_weight_map
    r=0
    g=0
    b=0
    display_name = node_feature_indices_map[node]
    if display_name in node_dialect_weight_map:
        weight_list = node_dialect_weight_map[display_name]
        for dialect_weight in weight_list:
            if 'dialect_0' in dialect_weight:
                r= int(dialect_weight.split("-")[1])
            if 'dialect_1' in dialect_weight:
                g= int(dialect_weight.split("-")[1])
            if 'dialect_2' in dialect_weight:
                b= int(dialect_weight.split("-")[1])
        r = min(r*10, 255)
        g = min(g*10, 255)
        b = min(b*10, 255)
        val = "rgb(" + str(r) + "," + str(g) + ","+ str(b)+")"
        return val
    else:
        return 'rgb(0,0,0)'

def get_node_color(node, node_count_map, node_feature_indices_map, node_color_scheme):
    #Select node coloring
    if node_color_scheme==0:
        # default uniform
        return 1
    elif node_color_scheme==1:
        return color_by_weight(node, node_count_map)
    elif node_color_scheme==2:
        return color_by_dialect(node, node_feature_indices_map)
    elif node_color_scheme==3:
        return color_by_dialect_number(node, node_feature_indices_map)
    elif node_color_scheme==4:
        return color_by_dialect_gradient(node, node_feature_indices_map)
    elif node_color_scheme==5:
        return color_by_dialect_gradient_hex(node, node_feature_indices_map)

###Functions for getting Object names######
def load_obj_names_file(filename):
    f = open(filename, "r")
    return f.readlines()


def get_all_edges(node, node_list):
    edges = set()
    sub_list = node.split(",")

    for display_name in node_list:
        test_list = display_name.split(",")
        if(all(x in sub_list for x in test_list)):
            edges.add(display_name)

    return edges
####################################################
### Main functions for generating dowker #########

def process_error_matrix(error_matrix_file_loc, name, directory, swap_common_messages, layout_scheme, node_color_scheme, node_size_scheme, edge_color_scheme):
    
    error_matrix = scipy.sparse.load_npz(error_matrix_file_loc)
    error_matrix = error_matrix.todense()
    
    total_files=error_matrix.shape[1]-1
    

    totals = error_matrix.transpose().sum(axis=0)
    
    totals=totals.tolist()[0]

    errors_to_reverse = list()

    if swap_common_messages:
        for x in range(1,total_files+1):
            for y in errors_to_reverse:
                if error_matrix[y,x]==1:
                    error_matrix[y,x]=0
                elif error_matrix[y,x]==0:
                    error_matrix[y,x]=1
                else:
                    print(error_matrix[y,x])
                    print("ERROR: non-binary value")
    

    totals = error_matrix.transpose().sum(axis=0)
    
    node_count_map= dict()

    #Create dictionary of nodes: list of fileRows
    node_rows_map = dict()
    
    #Create dictionary for readability of nodes:"feature index 1, feature index 2..." 
    node_feature_indices_map = dict()

    #Create dictionary of nodeString: connected nodeString, where nodes are connected if there is only a single additional feature between them. For example, 2,3 connects to 3 and 2, 1,2,3 connects to 2,3; 1,3; and 1,2
    node_edge_map = dict()

    # Create dictionary of nodeString: all lower level nodes
    node_full_edge_map = dict()
    
    #Create dictionary of node length: list of nodes strings
    length_node_map = dict()

    #There is no file 0, so the first file is index 1
    file_index=1;
    i=0
    #iterate over all columns representing each file's error vector
    for file_row in error_matrix.transpose().A:
        if i==0:
            i+=1
            continue
        row_hash_str = binary_row_hash(file_row)
        
        pg_row_str = process_node_cached(file_row, row_hash_str)
        

        i+=1
        if (i % 1000) ==0:
            #Print to assure the code is still running
            print("At row " + str(i))
        if len(pg_row_str.split(","))>MAXLEN:
            continue

        if row_hash_str in node_rows_map:
            node_rows_map[row_hash_str].append(file_index)
            node_count_map[row_hash_str]= node_count_map[row_hash_str]+1
        else:

            node_rows_map[row_hash_str]=list()
            node_count_map[row_hash_str]=1
            node_feature_indices_map[row_hash_str] = pg_row_str
            node_rows_map[row_hash_str].append(file_index)
 

            #Create all edges for complex, where an edge exists between each set of nodes
            #with a difference of 1 error
            node_edge_map[ pg_row_str ] = set()
     

            for index in range(len(file_row)):
                if file_row[index]==1:
                    conn_node = file_row.tolist()
                    conn_row = file_row.copy()
                    conn_row[index]=0
                    row_hash_str2 = binary_row_hash(conn_row)
                    conn_str = process_node_cached(conn_row, row_hash_str2) 
                    node_edge_map[pg_row_str].add(conn_str)
                   
        file_index+=1
    

    #Build up length node map for node positioning
    all_nodes=list()
    for key in node_feature_indices_map:
        
        display_name = node_feature_indices_map[key]

        group_len=len(display_name.split(","))
        if group_len<MAXLEN:
            all_nodes.append(display_name)
        if display_name=="":
            group_len=0
        if group_len in length_node_map:
            length_node_map[group_len].append(display_name)
        else:
            length_node_map[group_len]= list()
            length_node_map[group_len].append(display_name)

        #Also build up full edge map
    for node in all_nodes:
        
        node_full_edge_map[node] = get_all_edges(node, all_nodes)
        node_full_edge_map[node].add("")

    #position all nodes from shorter length of errors to longer, and sorted

    #Fill out string_pos_map, where each node to be displayed is mapped to a coordinate tuple (x,y,z)
    #Pass in the list of nodes, length_node_map, and node_edge_map, not all of which are necessarily used
                       
    string_pos_map = create_string_pos_map(all_nodes, length_node_map, node_edge_map, layout_scheme)

    # if we generate dialects, fill in node_count_map
    global node_weight_left_map 
    node_weight_left_map = node_count_map.copy()
    #Pass all dictionaries and display options to drawDowker
    draw_dowker(name, directory, node_count_map, node_feature_indices_map, node_rows_map, node_edge_map, string_pos_map, length_node_map, node_full_edge_map, layout_scheme, node_color_scheme, node_size_scheme, edge_color_scheme)

def weight_left(node_feature_indices_map):
    global node_weight_left_map
#    print(node_weight_left_map)
    count =0
    for node in node_weight_left_map:
        if node_weight_left_map[node]>0 and len(node_feature_indices_map[node].split(",")) < MAXLEN:
            count +=1
    if count>0:
        return True
    else:
        return False

def add_messages_to_dialect(dialect, node):
    global dialect_messages_map
    msgs = node.split(",")
    for msg in msgs:
        if (len(msg) > 0):
            dialect_messages_map[dialect].add(msg)
    return

def find_intersection(node1, node2, node_threshold):
    msgs1 = node1.split(",")
    msgs2 = node2.split(",")
    if len(msgs1)>1:
        return len([value for value in msgs1 if value in msgs2])>node_threshold
    else:
        return True

    

def expand_dialect(feature_indices_to_node_map, node_upper_node_map, node_full_edge_map, node_feature_indices_map, length_node_map, node_count_map):
    ##############################
        
    global node_weight_left_map
    global node_dialects_map
    global node_dialect_weight_map
    global dialect_messages_map
    global dialect_files_map
    ##############################
    max_length = max(length_node_map.keys())+1
    curr_dialect_index = 1

    # Sort length node map by either increasing or decreasing weight
    for length in length_node_map.keys():
        length_node_map[length].sort(key=lambda x: node_count_map[feature_indices_to_node_map[x]], reverse=True)


    while (weight_left(node_feature_indices_map)):

        dialect = "dialect_" + str(curr_dialect_index)
        curr_dialect_index +=1
        dialect_messages_map[dialect]= set()
        dialect_files_map[dialect] = 0
 
        index = 0
        
        for length in range(0, int(max_length)):
            if not(length in length_node_map):
                continue
            for display_name in length_node_map[length]:
                curr_node = feature_indices_to_node_map[display_name]
                node_weight=node_weight_left_map[curr_node]
                if not(dialect in node_dialects_map[display_name]) and node_weight>0:
                    if index>0:
                        #Not the first tree in this iteration
                        dialect = "dialect_" + str(curr_dialect_index)
                  #      print(dialect + ":" + display_name.replace(",", ".") + ":" + str(node_weight))
                        
                        dialect_messages_map[dialect]= set()
                        dialect_files_map[dialect] = 0
                        curr_dialect_index +=1
                    #print("curr node_weight" + str(node_weight))
                 #   print(dialect + ":" + display_name.replace(",", ".") + ":" + str(node_weight))
                        
                    dialect_weight = expand_node(display_name, dialect, node_weight, node_full_edge_map, feature_indices_to_node_map)
                    node_dialects_map[display_name].append(dialect)
                    node_dialect_weight_map[display_name].append(dialect+"-"+str(dialect_weight))
                    add_messages_to_dialect(dialect, display_name)
                    dialect_files_map[dialect] += dialect_weight
                    weight =max(node_weight - dialect_weight, 0)
                    node_weight_left_map[curr_node] =weight
                   # print("Set node " + display_name + " " + dialect + " " + str(dialect_weight) + " weight left " + str(node_weight_left_map[curr_node]))

                    conn_names = node_upper_node_map[display_name]
                    conn_names.sort(key=len)
                    for conn_name in conn_names:
                        
                        conn_node = feature_indices_to_node_map[conn_name]                                               
                        node_weight = node_weight_left_map[conn_node]
                        if node_weight==0:
                            continue
                        
                        max_weight = min(node_weight, dialect_weight)
                        tmp_weight = expand_node(conn_name, dialect, max_weight, node_full_edge_map, feature_indices_to_node_map)
                        weight_added= min(node_weight, tmp_weight)
                        if weight_added >0:
                            node_dialects_map[conn_name].append(dialect)
                            if tmp_weight > node_weight:
                                print("ERROR!")
                                input()
                            weight =max(node_weight - tmp_weight, 0)
                        
                            node_weight_left_map[conn_node] =weight
                            node_dialect_weight_map[conn_name].append(dialect+"-"+str(weight_added))
                            #print("Set node " + conn_name + " " + dialect + " " + str(weight_added) + " weight left " + str(node_weight_left_map[conn_node]))
                           # add_messages_to_dialect(dialect, conn_name)
                            
                            dialect_files_map[dialect] += tmp_weight

                    index+=1

        


def expand_node(name, dialect, dialect_weight, node_full_edge_map, feature_indices_to_node_map):
    global node_dialects_map
    global node_dialect_weight_map
    global node_weight_left_map
        
    min_weight = dialect_weight
    if dialect in node_dialects_map[name]:
        found=False
        for dialect_weight in node_dialect_weight_map[name]:
            if dialect in dialect_weight:
                found=True
                weight = int(dialect_weight.split("-")[1])
                return min(weight, dialect_weight)

        return min_weight
    
   
    edges = list(node_full_edge_map[name])
    edges.sort(key=len)

    for conn_name in edges:
        
        if conn_name==name:
            continue
        if not(conn_name in feature_indices_to_node_map):
            continue
        
        curr_node = feature_indices_to_node_map[conn_name]


        
        if conn_name in node_dialects_map:
            if dialect in node_dialects_map[conn_name]:
                found=False
                for dialect_weight in node_dialect_weight_map[conn_name]:
                    if dialect in dialect_weight:
                        found=True
                        weight = int(dialect_weight.split("-")[1])
                        if min_weight > weight:
                            min_weight = weight
     
            else:
                continue


    return min_weight
                        

            
def add_to_dialect(node_list, weight_this_branch, curr_dialect, feature_indices_to_node_map, node_upper_node_map):
    global node_weight_left_map
    global node_dialects_map
    global node_dialect_weight_map
    while (len(node_list)>0):
        
        display_name = node_list.pop(0)
  
        curr_node = feature_indices_to_node_map[display_name]
        node_weight=node_weight_left_map[curr_node]

        if not(curr_dialect in node_dialects_map[display_name]) and node_weight>0:
            dialect_weight = min(weight_this_branch, node_weight)                              
            #remove weight left at this node
            node_weight_left_map[curr_node] = max(node_weight - dialect_weight, 0)
            conn_nodes = copy.deepcopy(node_upper_node_map[display_name])
            conn_nodes.sort(key=len)
            node_dialects_map[display_name].append(curr_dialect)
            node_dialect_weight_map[display_name].append(curr_dialect+"-"+str(dialect_weight))
            # Add this node's messages to dialect
            #add_messages_to_dialect(curr_dialect, display_name)
            
            #Recurse on connected nodes
            if dialect_weight > 0:
                
                add_to_dialect(conn_nodes, dialect_weight, curr_dialect, feature_indices_to_node_map, node_upper_node_map)
    return
def draw_dowker(name, directory, node_count_map,  node_feature_indices_map, node_rows_map, node_edge_map, string_pos_map, length_node_map, node_full_edge_map, layout_scheme, node_color_scheme, node_size_scheme, edge_color_scheme):
    global node_dialects_map
    global dialect_messages_map
    global dialect_files_map
    #Given the dictionaries of node:properties, fill in the dowker 

    #lists for plotly to generate dowker, filling in each node and its properties sequentially

    #Node names
    nodes = []

    #Node display names
    labels = []

    #Number of files at each node
    weights = []

    #Rows of the error matrix corresponding to that node
    row_indices = []

    #Names for objects corresponding to the row indices
    obj_names = []
    
    #Node color
    colors=[]

    #Edges as a list of (starting node name, ending node name, node color)
    Edges=[]

    #Node sizes
    sizes=[]

    #Node X, Y, Z coordinates respectively
    Xn=[]
    Yn=[]
    Zn=[]

    #Dialects formatted for display
    dialects =[]
    

    feature_indices_to_node_map = {v: k for k, v in node_feature_indices_map.items()}

    node_upper_node_map = dict()
    # Populate the dictionary with each node: empty list
    for node in node_count_map:
        display_name = node_feature_indices_map[node]
        node_dialects_map[display_name]=list()
        node_dialect_weight_map[display_name] = list()
 
        node_upper_node_map[str(display_name)] = list()


    for node in node_count_map:
        display_name = node_feature_indices_map[node]
        if display_name in node_edge_map:
            for conn_node in node_edge_map[display_name]:
                
                if conn_node in feature_indices_to_node_map  and display_name in feature_indices_to_node_map:
                    if conn_node in feature_indices_to_node_map:
                        if not(display_name in node_upper_node_map[conn_node]) and not(display_name == conn_node):
                            
                          #  conn_node_display_name = node_feature_indices_map[conn_node]
                            #If both nodes exist, add to conn node dictionary
                            node_upper_node_map[str(conn_node)].append(display_name)
                            
                            #print("adding " + conn_node + "->" + display_name)
            #Also add node_full_edge_map features
            for conn_node in node_full_edge_map[display_name]:
                 if conn_node in feature_indices_to_node_map  and display_name in feature_indices_to_node_map:
                    if not(display_name in node_upper_node_map[conn_node]) and not(display_name == conn_node):            
                        node_upper_node_map[str(conn_node)].append(display_name)
                       # print("adding " + conn_node + "->" + display_name)
    
    # Code to assign all files/nodes to a dialect
    expand_dialect(feature_indices_to_node_map, node_upper_node_map, node_full_edge_map, node_feature_indices_map, length_node_map, node_count_map)

    #Get node display info and populate lists
    for node in node_count_map:

        display_name = get_display_name(node, node_feature_indices_map)

        if display_name in string_pos_map: 
   
            #Add display name to nodes
            nodes.append(display_name)

            #Add corresponding weight at node
            weights.append(node_count_map[node])
            
            #Add corresponding node size 
            node_size = get_node_size(node, display_name, node_count_map, node_size_scheme)
            sizes.append(node_size)

            #Add corresponding node color
            color = get_node_color(node, node_count_map, node_feature_indices_map, node_color_scheme)
            colors.append(color)


            #Add row indices (may be used for displaying files at node)
            row_indices.append(node_rows_map[node])
#            lines = load_obj_names_file("C:/Users/letitia.li/Documents/Safedocs/NITF/nitf-filelist.txt")
#            lines = load_obj_names_file("C:/Users/letitia.li/Documents/Safedocs/TA1/Eval/evalFour_filelist.txt")
  

            
            #Add dialects
            dialects.append(node_dialect_weight_map[display_name])
           # if len(filenames)>0:
            #    colors.append(20)
            #else:
             #   colors.append(0)
            #Add coordinates
            Yn.append(string_pos_map[display_name][1])
            Xn.append(string_pos_map[display_name][0])
            Zn.append(string_pos_map[display_name][2])
    for node in node_count_map:
        display_name = node_feature_indices_map[node]
        if display_name in node_edge_map:
            for conn_node in node_edge_map[display_name]:
                if conn_node in nodes and display_name in nodes:
                    if conn_node in feature_indices_to_node_map:
                        if feature_indices_to_node_map[conn_node] in node_count_map:
                          #  conn_node_display_name = node_feature_indices_map[conn_node]
                            #If both nodes exist, add edge
                            edge_color = get_edge_color(node, feature_indices_to_node_map[conn_node], node_count_map, edge_color_scheme)
                            #Also add edge to node_uppernode_map for dialect detection
                            Edges.append((nodes.index(display_name), nodes.index(conn_node), edge_color))


    
    L=len(Edges)
    N=len(nodes)
 
    max_color = max(colors)
    min_color = min(colors)

    Xe=[]
    Ye=[]
    Ze=[]
    edge_colors=[]
    for e in Edges:
        Xe+=[Xn[e[0]],Xn[e[1]], None]# x-coordinates of edge ends
        Ye+=[Yn[e[0]],Yn[e[1]], None]
        Ze+=[Zn[e[0]],Zn[e[1]], None]

        #For some reason, we need to add edge color 3 times
        edge_colors.append(e[2])
        edge_colors.append(e[2])
        edge_colors.append(e[2])
  
    trace1=go.Scatter3d(x=Xe,
                   y=Ye,
                   z=Ze,
                   mode='lines',
                   line=dict(color=edge_colors, width=1),
                   hoverinfo='none'
                   )

#Fill in text with the fields in the hover text. In the example, groupings is clipped to 50 characters so as not to run off the edge of the hover box
    trace2=go.Scatter3d(x=Xn,
                   y=Yn,
                   z=Zn,
                   mode='markers',
                   name='',
                   marker=dict(symbol='circle',
                                size=sizes,
                             #   cmax=max_color,
                              #  cmin=min_color,
                                color=colors,
                                line=dict(color='rgb(50,50,50)', width=0.5)
                                ),
                   text= ['Node {}<br> Weight {} <br> Dialects {}'.format(nodes[i][:50],weights[i], dialects[i][:50]) for i in range(0,N)],
                    hovertemplate =
        '<i>%{text}'+
        '<br>Length: %{z}<br>',
                   hoverinfo='text',

                   )

    axis=dict(showbackground=False,
              showline=False,
              zeroline=False,
              showgrid=False,
              showticklabels=False,
              title=''
              )

    layout = go.Layout(
             title="Dowker complex",
             width=1000,
             height=1000,
             showlegend=False,
             scene=dict(
                 xaxis=dict(axis),
                 yaxis=dict(axis),
                 zaxis=dict(axis),
            ),
         margin=dict(
            t=100
        ),
        hovermode='closest',
       )
    data=[trace1, trace2]
    fig=go.Figure(data=data, layout=layout)

       
    plotly.offline.plot(fig, filename=directory+name+'dowker.html')


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description='Build a dowker from a scipy dense error matrix')
    parser.add_argument('error_matrix',
                        help="Location of error matrix file")
    parser.add_argument('dowker_name', help="Name of testset and dowker file")
    parser.add_argument('-o', '--output_directory', default='./',
                        help="Directory to store output files")
    parser.add_argument('-swap', action='store_true')
    
    # Prompt for layout
    print("Select layout:")
    print(" 0: Circular")
    print(" 1: Group by neighbors")
    print(" 2: 2D spring layout")
    print(" 3: Force-directed")
    print(" 4: Force-directed by layer")
    
    layout_index = input()
    if (layout_index.isdigit() and int(layout_index)< 5):
        layout_index=int(layout_index)
    else:
        layout_index=0


    # Prompt for Node Color
    print("Select node color scheme:")
    print(" 0: Uniform")
    print(" 1: Color by weight")
    print(" 2: Color by max dialect number")
    print(" 3: Color by number of dialects at node")  
    print(" 4: Color by gradient of 3 dialects in rgb")
    print(" 5: Color by gradient of 6 dialects in hex")
    node_color_index = input()
    if (node_color_index.isdigit() and int(node_color_index)< 6):
        node_color_index=int(node_color_index)
    else:
        node_color_index=0

    # Prompt for Node Size
    print("Select node sizing scheme:")
    print(" 0: Uniform")
    print(" 1: Weight")
    node_size_index = input()
    if (node_size_index.isdigit() and int(node_size_index)< 2):
        node_size_index=int(node_size_index)
    else:
        node_size_index=0    

    # Prompt for Edge Color
    print("Select edge color scheme:")
    print(" 0: Uniform")
    print(" 1: Consistent (green) vs Inconsistent (red)")
    edge_color_index = input()

    if (edge_color_index.isdigit() and int(edge_color_index)< 2):
        edge_color_index=int(layout_index)
    else:
        edge_color_index=0

    args = parser.parse_args()    
    swap = args.swap
    process_error_matrix(args.error_matrix, args.dowker_name, args.output_directory, args.swap, layout_index, node_color_index, node_size_index, edge_color_index)



