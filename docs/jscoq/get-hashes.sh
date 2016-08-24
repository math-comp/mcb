#!/bin/bash

function get_hash {
  git --git-dir ${2}/.git log --pretty=format:"+ ${1}: %s%n  %H" -n 1
}

function get_hg_hash {
  git --git-dir ${2}/.git log --pretty=format:"+ ${1}: %s%n  %H" -n 1
}

function get_svn {
  echo "+ ${1}: rev `svnversion ${2}`"
}

JSCOQ_DIR=~/research/jscoq/
COQ_DIR=~/external/coq-git-32/
MC_DIR=~/external/coq/math-comp/
CM_DIR=${JSCOQ_DIR}/external/CodeMirror
FLOCQ_DIR=~/external/coq/flocq/
CT_DIR=~/external/coq/coquelicot/
TLC_DIR=~/external/coq/tlc/
SF_TAR=~/external/coq/sf.tar.gz
COLOR_DIR=~/external/coq/CoLoR-8.5
HOTT_DIR=~/external/HoTT/
UNIM_DIR=~/external/coq/UniMath/
MC_DIR=~/external/coq/mirror-core/
CEL_DIR=~/external/coq/coq-ext-lib/

function get_build_name {
  COQH=`git --git-dir ${COQ_DIR}/.git log --pretty=format:"%h" -n 1`
  JSCOQH=`git --git-dir ${JSCOQ_DIR}/.git log --pretty=format:"%h" -n 1`
  echo "Build: $JSCOQH/$COQH"
  echo
}

function get_date {
  echo "+ ${1}: `stat -c '%y' ${2}`"
}

get_build_name

get_hash "coq"        $COQ_DIR
get_hash "jscoq"      $JSCOQ_DIR
get_hash "CodeMirror" $CM_DIR
get_hash "math-comp"  $MC_DIR
get_hash "flocq"      $FLOCQ_DIR
get_hash "coquelicot" $CT_DIR
get_hash "tlc"        $TLC_DIR
get_hash "HoTT"       $HOTT_DIR
get_hash "UniMath"    $UNIM_DIR
get_hash "coq-ext-lib"  $CEL_DIR
get_hash "mirror-core"  $MC_DIR
get_svn  "color"      $COLOR_DIR
get_date "sf"         $SF_TAR
