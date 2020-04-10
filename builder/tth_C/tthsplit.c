/*
Split a tth-produced MIME file into its consituent files. 
Copyright 1997 I.H.Hutchinson.
*/

#define LINELEN 256
#include <stdio.h>
#include <string.h>
main(argc,argv)
int argc;
char *argv[];
{
int slen;
char *ch,*ch2;
char bound[LINELEN]={0};
char buff[LINELEN]={0};
FILE *file;
if(argc > 1){ 
  printf( "Usage: tthsplit <infile \n Split a tth MIME file on stdin.\n"); 
  return 1;
}
do {
  ch2=fgets(buff,LINELEN,stdin); 
  if((ch=strstr(buff,"MULTIPART/MIXED; BOUNDARY="))!=NULL){
    ch=strstr(ch,"\"")+1;
    *(strstr(ch,"\""))=0;
    strcpy(bound,"--");
    strcat(bound,ch);
  }
} while (ch2 != NULL && *buff != 13 && *buff != 10); /* header lines */
 if(!*(bound)){
   fprintf(stderr,"File does not start with MIME boundary definition\n");
   exit(2);
 }
file=NULL;
while(fgets(buff,LINELEN,stdin) != NULL){
  if(strstr(buff,bound)==buff){
    /*printf("Found bound. file=%d\n",(int)file); */
    fgets(buff,LINELEN,stdin);
    if(file!=NULL) fclose(file);
    if((ch=strstr(buff,"name=\"")) != NULL){
      ch=ch+6;
      *(strstr(ch,"\""))=0;
      if((file=fopen(ch,"w"))!=NULL){
	fprintf(stderr,"%s\n",ch);
      }else{
	fprintf(stderr,"Can't open %s\n",ch);
	return 2;
      }
    }else{
      fprintf(stderr,"Can't find file name in:\n%s",buff);
      return 3;
    }
  } else fputs(buff,file);
}
fclose(file);
}



