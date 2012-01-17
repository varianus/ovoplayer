Name:ovoplayer
Version:1.0
Release:1
Summary:Free multiplatform audio player.
Group: Applications/Multimedia
BuildRoot:/var/tmp/%{name}-%{version}-%{release}-root
License:GPL
Source:ovoplayer.tar.gz
URL:http://code.google.com/p/ovoplayer
Packager:ovoplayer
Requires:mplayer | VLC

%description
OVOPLAYER

#%prep
#%setup –c
#%install
#install -m 755 ovoplayer /usr/bin/ovoplayer
rm -rf $RPM_BUILD_ROOT
#mkdir -p $RPM_BUILD_ROOT
cp ovoplayer /usr/local/bin/ovoplayer

%pre/%post/%trigger
cp ovoplayer /usr/local/bin/ovoplayer

%files
#/usr/bin/
%defattr(-,root,root)
/ovoplayer
#(-,root,root)
#%{userpath}

