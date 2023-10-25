# This file is part of DRACO.
# SPDX-Identifier: LGPL-3.0-or-later
#
# DRACO is free software: you can redistribute it and/or modify it under
# the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# DRACO is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with DRACO.  If not, see <https://www.gnu.org/licenses/>.

project(
  'draco',
  'fortran',
  version: files('version.txt'),
  meson_version: '>=0.60.0',
  default_options: [
    'buildtype=debugoptimized',
    'default_library=static',
  ],
)

lib_deps = []
subdir('config')

srcs = []
subdir('src')

draco_lib = library(
  meson.project_name(),
  sources: srcs,
  version: meson.project_version(),
  dependencies: lib_deps,
  install: true,
  link_language: 'fortran',
)
draco_dep = declare_dependency(
  link_with: draco_lib,
  dependencies: lib_deps,
)

inc_dir=include_directories('.')
subdir('app')

module_id = meson.project_name() / fc_id + '-' + fc.version()
meson.add_install_script(
  find_program(files('config'/'install-mod.py')),
  get_option('includedir') / module_id,
)

pkg = import('pkgconfig')
pkg.generate(
  draco_lib,
  subdirs: ['', module_id],
)

commit = get_option('build_name')                                                                                                                                                              
git = find_program('git', required: false)                                                                                                                                                     
if git.found()                                                                                                                                                                                 
  git_commit = run_command(git, 'show', '-s', '--format=%h')                                                                                                                                   
  if git_commit.returncode() == 0                                                                                                                                                              
    commit = git_commit.stdout().strip()                                                                                                                                                       
  endif                                                                                                                                                                                        
endif  

pymod = import('python')                                                                                                                                                                       
python = pymod.find_installation('python3', required: false)                                                                                                                                   
if not python.found()                                                                                                                                                                          
  python = find_program('python3', 'python')                                                                                                                                                   
endif 

config = configuration_data({                                                                                                                                                                  
  'name': meson.project_name(),                                                                                                                                                                
  'description': 'Dynamic Radii Adjustment for Continuum Solvation',                                                                                                                       
  'version': meson.project_version(),                                                                                                                                                          
  'commit': commit,                                                                                                                                                                            
  'date': run_command(python, '-c', 'import datetime; print(datetime.date.today().strftime("%Y-%m-%d"))').stdout().strip(),                                                                    
  'author': run_command(python, '-c', 'import getpass; print(getpass.getuser())').stdout().strip(),                                                                                            
  'origin': run_command(python, '-c', 'import socket; print(socket.gethostname())').stdout().strip(),                                                                                          
})                                                                                                                                                                                             
                                                                                                                                                                                               
configure_file(                                                                                                                                                                                
  input: files('config/version.f90'),                                                                                                                                                
  output: 'draco_version.fh',                                                                                                                                                                    
  configuration : config,                                                                                                                                                                      
)  

#subdir('test')
