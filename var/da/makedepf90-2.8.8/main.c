/* 
 * Copyright (C) 2000-2006 Erik Edelmann <Erik.Edelmann@iki.fi>
 *
 *     This program is free software;  you  can  redistribute  it
 *     and/or modify it under the terms of the GNU General Public
 *     License version 2 as published  by  the  Free  Software  
 *     Foundation.
 *
 *     This program is distributed in the hope that  it  will  be
 *     useful, but WITHOUT ANY WARRANTY; without even the implied
 *     warranty of MERCHANTABILITY or FITNESS  FOR  A  PARTICULAR
 *     PURPOSE.   See  the  GNU  General  Public License for more
 *     details.
 *
 *     You should have received a copy of the GNU General  Public
 *     License along with this program; if not, write to the Free
 *     Software Foundation, Inc., 59  Temple  Place,  Suite  330,
 *     Boston, MA  02111-1307  USA
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <fnmatch.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>
#include "list.h"
#include "finddep.h"
#include "utils.h"
#include "errormesg.h"
#include "modfile_name.h"
#include "global.h"
#include "macro.h"
#include "xmalloc.h"

Options options;


typedef struct {
    /* File Specific Rules */
    char *file;
    char *rule;
} Fsr;


static int fsr_strcmp(const char *file, const Fsr *rule)
{
    return fnmatch(rule->file, file, FNM_PATHNAME);
}


static const char helpstring[] =
    "\nUsage: makedepf90 [options] sourcefile(s)\n\n" 
    "Options:\n"
    "\n-h\tprint this message to stdout and quit.\n"
    "\n-V\tprint version and copyright information to stdout and quit.\n"
    "\n-W\tprint warning messages about missing includes/modules.\n"
    "\n-m fmt\tWrite mod-file names using the format 'fmt'.\n"
          "\t'fmt' may contain any of the following modifiers:\n"
          "\t  '%%' for '%'\n"
          "\t  '%f' for name of the source file (without suffix),\n"
          "\t  '%m' for 'modulename' (in lowercase)\n"
          "\t  '%M' for 'MODULENAME' (in uppercase).\n"
          "\tDefault: \"%f.o\".\n"
    "\n-u module\n\tIgnore modules named 'module'.\n"
    "\n-d file\tMake all targets dependent on 'file'.\n"
    "\n-r rule\tAdd 'rule' (indented by a tab) to all dependency lines,\n"
        "\texcept lines given rule(s) with the -R option below.\n"
        "\t'rule' may contain the modifiers:\n"
        "\t  '%f' for the name (without suffix) of the source file that\n"
        "\t       line was created for.\n"
        "\t  '%%' for '%'\n"
    "\n-R pattern rule\n\tAdd 'rule' (indented by a tab) to the dependency\n"
        "\tline for files matching 'pattern', where 'pattern' is a\n"
        "\t\"shell pattern\", i.e it may contain the wildcards\n"
        "\t  '*' for any number of any characters\n"
        "\t  '?' for any character\n"
        "\t  '[abc]' for any of 'a', 'b' or 'c'.\n"
    "\n-fixed\tTreat all files as fixed format.\n"
    "\n-free\tTreat all files as free format.\n"
    "\n-o name\tCreate a dependency line + rule for linking the final\n"
        "\texecutable 'name'.\n"
    "\n-l rule\tUse 'rule' for linking the executable.\n"
        "\tDefault: $(FC) -o $@ $(FFLAGS) $(LDFLAGS) $(FOBJ) $(LIBS)\n"
    "\n-coco\tLook for coco set-files.  Implies '-free'.\n"
    "\n-D NAME\tDefine pre-processor symbol 'NAME'\n"
    "\n-b PATH\tAssume object files are placed in PATH.\n"
    "\n-nosrc\tRemove the explicit dependency on the source file\n"
    "\n-I PATH1:PATH2:...\n\tSearch path(s) for source files\n"
    "\n\nReport bugs to erik.edelmann@iki.fi\n";


static char ver[]="makedepf90 version " VERSION;


static const char license[]=
    "\nCopyright (C) 2000--2006 Erik Edelmann <Erik.Edelmann@iki.fi>\n"
    "\n"
    "makedepf90 is free software; you can redistribute it and/or modify\n"
    "it under the terms of the GNU General Public License version 2 as\n"
    "published by the Free Software Foundation.\n"
    "\n"
    "This program is distributed in the hope that it will be useful,\n"
    "but WITHOUT ANY WARRANTY; without even the implied warranty of\n"
    "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU\n"
    "General Public License for more details.\n"
    "\n"
    "You should have received a copy of the GNU General Public License\n"
    "along with this program; if not, write to the Free Software Foundation,\n"
    "Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.";


int main (int argc, char **argv)
{
    int i, j, n;
    List *files = NULL;
    char *s, *srcfile;
    Dependency *dep;
    List *deplist = NULL;   /* Dependencies */
    List *modlist = NULL;   /* Defined modules */
    List *extradeps = NULL; /* "Extra" dependencies (given by the '-d' option */
    List *rules = NULL;  /* List of rules to be added to all dependency lines */
    List *fspecrules = NULL; /* FileSpecific rules */
    List *obj = NULL;
    List *macrolist = NULL;  /* -D MACRO */
    Module *mod;
    List *h1, *h2;

    set_progname(argv[0]);

    if (argc == 1)  {
        printf ("%s", helpstring);
        exit (EXIT_SUCCESS);
    }

    /* Set a few option defaults */
    options.warn_missing = false;
    options.modfile_fmt = (char *)MODFILE_FMT_DEFAULT;
    options.src_fmt = SUFFIX;
    options.create_obj = false;
    options.exe_name = NULL;
    options.link_rule = (char *)LINK_RULE_DEFAULT;
    options.coco = false;
    options.obj_dir_set = false;
    options.obj_dir = "";
    options.src_dep = true;
    options.ignore_mods = NULL;
    options.src_path = NULL;

    /* Parse command line arguments */
    for (i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "--help") == 0) {
            printf ("%s", helpstring);
            exit (EXIT_SUCCESS);

        } else if (strcmp(argv[i], "-V") == 0 
               || strcmp(argv[i], "--version") == 0) {
            printf("%s\n", ver);
            printf("%s\n", license);
            exit (EXIT_SUCCESS);

        } else if (strcmp(argv[i], "-W") == 0 
                   || strcmp(argv[i], "-Wmissing") == 0) {
            options.warn_missing = true;

        } else if (strcmp(argv[i], "-Wconfused") == 0) {
            options.warn_confused = true;

        } else if (strncmp(argv[i], "-m", 2) == 0) {
            if (strlen(argv[i]) == 2) {
                if (i == argc-1)  fatal_error("Option '-m' needs argument");
                options.modfile_fmt = xstrdup(argv[++i]);
            } else
                options.modfile_fmt = xstrdup(&(argv[i][2]));

        } else if (strncmp (argv[i], "-u", 2) == 0) {
            if (strlen(argv[i]) == 2) {
                if (i == argc-1)  fatal_error("Option '-u' needs argument");
                s = xstrdup(argv[++i]);
            } else
                s = xstrdup(&(argv[i][2]));

            if (!list_find(options.ignore_mods, s, COMP_FUN(&strcasecmp)))
                options.ignore_mods = list_prepend(options.ignore_mods, s);

        } else if (strcmp(argv[i], "-fixed") == 0) {
            options.src_fmt = FIXED;

        } else if (strcmp(argv[i], "-free") == 0) {
            options.src_fmt = FREE;

        } else if (strncmp(argv[i], "-d", 2) == 0) {
            if (strlen(argv[i]) == 2) {
                if (i == argc-1)  fatal_error("Option '-d' needs argument");
                s = xstrdup(argv[++i]);
            } else
                s = xstrdup(&(argv[i][2]));

            if (!list_find(extradeps, s, COMP_FUN(&strcasecmp)))
                extradeps = list_prepend(extradeps, s);

        } else if (strncmp(argv[i], "-r", 2) == 0) {
            if (strlen(argv[i]) == 2) {
                if (i == argc-1)  fatal_error ("Option '-r' needs argument");
                s = xstrdup(argv[++i]);
            } else
                s = xstrdup(&(argv[i][2]));

            if (!list_find(rules, s, COMP_FUN(&strcasecmp)))
                rules = list_append(rules, s);
        
        } else if (strncmp(argv[i], "-R", 2) == 0) {
            Fsr *h;
            h = (Fsr *) xmalloc(sizeof(Fsr));
            if (strlen(argv[i]) == 2) {
                if (i == argc-1)  fatal_error("Option '-R' needs 2 arguments");
                h->file = xstrdup(argv[++i]);
            } else
                h->file = xstrdup(&(argv[i][2]));

            if (i == argc-1) fatal_error("Option '-R' needs 2 arguments");
            h->rule = xstrdup (argv[++i]);
            fspecrules = list_append(fspecrules, h);

        } else if (strncmp(argv[i], "-o", 2) == 0) {
            options.create_obj = true;
            if (strlen(argv[i]) == 2) {
                if (i == argc-1)  fatal_error("Option '-o' needs argument");
                options.exe_name = xstrdup(argv[++i]);
            } else
                options.exe_name = xstrdup(&(argv[i][2]));

        } else if (strncmp(argv[i], "-l", 2) == 0) {
            if (strlen(argv[i]) == 2) {
                if (i == argc-1)  fatal_error("Option '-l' needs argument");
                options.link_rule = xstrdup(argv[++i]);
            } else
                options.link_rule = xstrdup(&(argv[i][2]));

        } else if (strcmp(argv[i], "-coco") == 0) {
            options.coco = true;
            options.src_fmt = FREE;
                
        } else if (strncmp(argv[i], "-D", 2) == 0) {
            Macro *mac;
            char *eq;
            char *s;

            /* Copy the argument of -D (ignoring any '=definition') to a
             * 'Macro' and add it to 'macrolist'. */
            mac = macro_new ();
            if (strlen(argv[i]) == 2) {
                char *eq;
                char *s;

                if (i == argc-1)  fatal_error("Option '-D' needs argument");
                i++;

                eq = strchr(argv[i], '=');
                if (eq != NULL)
                    s = xstrndup(argv[i], eq - argv[i]);
                else
                    s = xstrdup(argv[i]);
                
                macro_setname(mac, s);
            } else {
                eq = strchr(&argv[i][2], '=');
                if (eq != NULL)
                    s = xstrndup(&argv[i][2], eq - argv[i] - 2);
                else
                    s = xstrdup(&argv[i][2]);
                
                macro_setname(mac, s);
            }
            if (!list_find(macrolist, mac, &macrocmp))
                macrolist = list_prepend(macrolist, mac);

        } else if (strncmp(argv[i], "-b", 2) == 0) {
            if (strlen(argv[i]) == 2) {
                if (i == argc - 1) fatal_error("Option '-b' needs argument");
                options.obj_dir = xstrdup(argv[++i]);
            } else
                if (argv[i][2] == '=') {
                    options.obj_dir = xstrdup(&(argv[i][3]));
                } else
                    options.obj_dir = xstrdup(&(argv[i][2]));

            n = strlen(options.obj_dir);
            if (n > 0 && options.obj_dir[n - 1] != '/') {
                options.obj_dir = xrealloc(options.obj_dir, n+2);
                strcat(options.obj_dir, "/");
            }

            options.obj_dir_set = true;

        } else if (strncmp(argv[i], "-I", 2) == 0) {
            int jp;

            if (strlen(argv[i]) == 2) {
                if (i == argc - 1) fatal_error("Option '-I' needs argument");
                s = xstrdup(argv[++i]);
            } else
                if (argv[i][2] == '=' ) {
                    s = xstrdup(&(argv[i][3]));
                } else
                    s = xstrdup(&(argv[i][2]));

            n = strlen(s);
            jp = 0;
            for(j = 0;j < n; j++){
               if (s[j] == ' ' || s[j]==':') {
                 options.src_path = list_append(options.src_path, 
                                                xstrndup(s+jp, j-jp));
                 jp = j + 1;
               } else if (j == n-1) {
                 options.src_path = list_append(options.src_path,xstrdup(s+jp));
               }
            }

        } else if (strcmp(argv[i], "-nosrc") == 0) {
            options.src_dep = false;

        /*
         * Add new options here
         */

        } else if (*argv[i] == '-') {
            fatal_error("Unknown Option '%s'", argv[i]);

        } else {
            /* Gee, we got a filename! */
            if (!list_find(files, argv[i], COMP_FUN(&strcasecmp)))
                files = list_prepend(files, argv[i]);
        }
    }

    /* Parse the files, creating target and dependency lists. */
    for (h1 = files; h1; h1 = h1->next) {
        char *tmp;

        dep = dependency_new();
        srcfile = (char *)h1->data;
        if (find_dep(srcfile, dep, &modlist, macrolist)) {
            dep->sourcefile = srcfile;

            tmp = replace_suffix(srcfile, ".o");
            if (!list_find(dep->targets, tmp, COMP_FUN(&strcasecmp)))
                dep->targets = list_prepend(dep->targets, tmp);
            else
                free(tmp);

            if (options.src_dep)
                dep->includes = list_prepend(dep->includes, srcfile);

            if (options.coco) {
                char *setfile;
                struct stat blah;

                setfile = replace_suffix(srcfile, ".set");
                if (stat(setfile, &blah) == 0) {
                    if (!list_find(dep->includes,setfile,COMP_FUN(&strcasecmp)))
                        dep->includes = list_append(dep->includes, setfile);
                } else if (stat("coco.set", &blah) == 0) {
                    free(setfile);
                    setfile = xstrdup("coco.set");
                    if (!list_find(dep->includes,setfile,COMP_FUN(&strcasecmp)))
                        dep->includes = list_append(dep->includes, setfile);
                } else
                    free(setfile);
            }

            deplist = list_prepend(deplist, dep);
        } else
            free(dep);

        if (options.create_obj)
            obj = list_prepend(obj, replace_suffix(srcfile, ".o"));
    }

    /* Print FOBJ macro and linking dependecy-line + rule. */
    if (options.create_obj) {
        printf("FOBJ=");
        for (h1 = obj; h1; h1 = h1->next)
            if (options.obj_dir_set)
                printf("%s ", set_path(h1->data, options.obj_dir));
            else
                printf("%s ", (char *)h1->data);
        printf("\n\n%s: $(FOBJ)\n\t%s\n\n", options.exe_name,options.link_rule);
    }

    /* Print the 'target: dependency' lists. */
    for (h1 = deplist; h1; h1 = h1->next) {
        bool byR = false;

        dep = (Dependency *)h1->data;

        /* If there are no dependencys, there is no need to output anything */
        if (list_length(dep->includes) + list_length(dep->modules) == 0)
            continue;

        /* Targets */
        for (h2 = dep->targets; h2; h2 = h2->next)  
            if (options.obj_dir_set)
                printf("%s ", set_path(h2->data, options.obj_dir));
            else
                printf("%s ", (char *)h2->data);

        printf(": ");;

        /* Includes */
        for (h2 = dep->includes; h2; h2 = h2->next)  
            printf("%s ", (char *)h2->data);

        /* Modules */
        for (h2 = dep->modules; h2; h2 = h2->next) {
            List *l;
            s = (char *)h2->data;

            if (!(l = list_find(modlist, s, &modstrcmp))) {
                /* Don't write *.mod-file to dependency list if its definition 
                 * isn't found. */
                if (options.warn_missing)  warning("Module '%s' not found", s);

            } else {
                mod = (Module *)l->data;
                if (strcasecmp(mod->sourcefile, dep->sourcefile) == 0) {
                    /* Dont' write *.mod-file to the dependency list if it 
                     * defined in the same file it is USEd from. */
                } else {
                    if (options.obj_dir_set)
                        printf("%s ", 
                               set_path(mod->modfile_name, options.obj_dir));
                    else
                        printf("%s ", mod->modfile_name);
                }
            }

        }

        /* Extra dependencies (given with the '-d' option */
        for (h2 = extradeps; h2; h2 = h2->next)  
            printf("%s ", (char *)h2->data);

        putchar('\n');

        /* Print -R rules (if there are any) */
        for (h2 = fspecrules; h2; h2 = h2->next) {
            Fsr *h = (Fsr *)h2->data;

            if (fsr_strcmp(dep->sourcefile, h) == 0) {
                char *tmp = expand_rule(h->rule, dep->sourcefile);
                printf("%s\n", tmp);
                free(tmp);
                byR = true;
            }
        }
            
        /* If the file wasn't given any rules by the -R option, print -r rules*/
        if (!byR) {
            for (h2 = rules; h2; h2 = h2->next) {
                char *tmp = expand_rule((char *)h2->data, dep->sourcefile);
                printf("%s\n", tmp);
                free(tmp);
            }
        }

    }

    exit(EXIT_SUCCESS);
}
