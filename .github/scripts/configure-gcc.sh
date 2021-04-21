sudo apt update

# Install gcc 7 and g++ 7
sudo apt-get install gcc-7 g++-7 g++-7-multilib libstdc++-7-doc  binutils-doc build-essential cpp-doc gcc-7-doc libstdc++6-7-dbg lib32stdc++6-7-dbg libx32stdc++6-7-dbg make autoconf automake libtool flex bison gdb gawk gcc-doc gfortran libgfortran3 libgcc1-dbg libgomp1-dbg libitm1-dbg libatomic1-dbg libasan4-dbg liblsan0-dbg libtsan0-dbg libubsan0-dbg libcilkrts5-dbg libmpx2-dbg libquadmath0-dbg glibc-doc python -y

# Redirect gcc and g++ to the installed versions
sudo update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-7 60 
sudo update-alternatives --install /usr/bin/g++ g++ /usr/bin/g++-7 60

# Redirect cc and c++ to the installed gcc and g++ versions:		
sudo update-alternatives --install /usr/bin/cc cc /usr/bin/gcc 30
sudo update-alternatives --set cc /usr/bin/gcc
		
sudo update-alternatives --install /usr/bin/c++ c++ /usr/bin/g++ 30
sudo update-alternatives --set c++ /usr/bin/g++

sudo apt install -y texinfo
sudo apt-get install -y dejagnu
./contrib/download_prerequisites

cd ..
mkdir objdir