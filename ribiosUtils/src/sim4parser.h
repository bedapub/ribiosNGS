/*****************************************************************************
* (c) Copyright 2012-2013 F.Hoffmann-La Roche AG                             *
* Contact: bioinfoc@bioinfoc.ch, Detlef.Wolf@Roche.com.                      *
*                                                                            *
* This file is part of BIOINFO-C. BIOINFO-C is free software: you can        *
* redistribute it and/or modify it under the terms of the GNU Lesser         *
* General Public License as published by the Free Software Foundation,       *
* either version 3 of the License, or (at your option) any later version.    *
*                                                                            *
* BIOINFO-C is distributed in the hope that it will be useful, but           *
* WITHOUT ANY WARRANTY; without even the implied warranty of                 *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU          *
* Lesser General Public License for more details. You should have            *
* received a copy of the GNU Lesser General Public License along with        *
* BIOINFO-C. If not, see <http://www.gnu.org/licenses/>.                     *
*****************************************************************************/
/** @file sim4parser.h
    @brief Purpose: dissect the output of the SIM4 program.
    Module prefix sim4p_
*/
#ifndef SIM4PARSER_H
#define SIM4PARSER_H

#ifdef __cplusplus
extern "C" {
#endif

void sim4p_register_begin (int (*f)(void));
void sim4p_register_name (int (*f)(char *file1,int length1,char *file2,
                                   char *dbseq,int length2));
void sim4p_register_exon (int (*f)(int qs,int qe,int ss,int se,int pctid,
                                   char *contigous,int complement));
void sim4p_register_fragmentseq (int (*f)(char *scale,char *query,char *match,
                                          char *subject,int ident,int sim,
                                          int qlen,int alen,
                                          int introndir,int complement));
void sim4p_register_exonseq (int (*f)(int qs,int qe,int ss,int se,char *q,
                                      char *m,char *s,int pctid,int pctsim,
                                      int complement));
void sim4p_register_intronseq (int (*f)(int qs,int qe,int ss,int se,char *q,
                                        char *m,char *s,int complement));
void sim4p_register_end (int (*f)(void));
void sim4p_init (void);
void sim4p_run (LineStream ls);

#ifdef __cplusplus
}
#endif

#endif
