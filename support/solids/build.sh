#!/bin/sh
# Build fancypencil-solids, the exact-solids helper, against Open CASCADE.
#   sh support/solids/build.sh                      (Ubuntu's libocct-*-dev packages)
#   OCCT=/path/to/occt/install sh support/solids/build.sh   (a build of your own)
# The program lands beside this script.
set -e
here=$(cd "$(dirname "$0")" && pwd)
if [ -n "$OCCT" ]; then
  INC="$OCCT/include/opencascade"; LIB="$OCCT/lib"
  RPATH="-Wl,-rpath,\$ORIGIN/../../lib"
else
  INC=/usr/include/opencascade; LIB=/usr/lib/x86_64-linux-gnu
  [ -d "$LIB" ] || LIB=/usr/lib
  RPATH=
fi
LIBS="-lTKSTEP -lTKSTEPAttr -lTKSTEPBase -lTKSTEP209 -lTKXSBase -lTKFeat -lTKOffset -lTKFillet -lTKMesh \
 -lTKBool -lTKBO -lTKShHealing -lTKPrim -lTKTopAlgo -lTKGeomAlgo -lTKBRep -lTKGeomBase -lTKG3d -lTKG2d -lTKMath -lTKernel"
g++ -O2 -std=c++17 -o "$here/fancypencil-solids" "$here/fancypencil-solids.cxx" -I"$INC" -L"$LIB" $RPATH $LIBS
echo "built $here/fancypencil-solids"
