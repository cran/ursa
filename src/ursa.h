// Genearated by $(R_USER)/ursa-package/src/packageRegistration_2.R

void focalMean(double *x,double *bg,int *dim,double *W,double *cvr,int *Z,int *E
            ,int *verbose,double *res);
void interp4(double *x,int *dim,int *win,double *cover,double *res);
int focalCommon(double *x,int *dim,double *bg,double *H,int *sz
                ,double *cvr,int *fz,int *E,int *verb,double *res);
void table_log(int *x,int *size,int *res);
void groupSummary(double *x,int *dim,double *_cover,double *weight,int *_fun
                 ,double *res);
void variability4(double *y,double *x,int *dim,double *cover,double *res);
void expand(double *x,int *dim,int *S,int *verb,double *res);
void dist2dist(double *x1,double *y1,double *x2,double *y2
              ,int *lenxy,int *lendf,int *pos,int *sphere,int *verb
              ,double *dist,int *ind);
void focalOsisaf(double *x,int *dim,double *bg,double *sz,double *S,double *A
               ,double *cvr,int *fz,int *E,int *verb,double *res);
void internalMarginWithBackground(double *x,double *bg,int *dim,int *indr,int *indc);
void focal4(double *x,double *bg,int *dim,int *S,int *F,double *cvr
            ,int *K,int *verbose,double *res);
void makeField(double *x,int *dim,int *res);
void isNear(double *x1,double *x2,int *len1,int *len2,int *res);
void readBsqLineInteger(char **fname,int *dim,int *index,int *nindex,int *dtype
                       ,int *byteorder,int *res);
void focalExtrem(double *x,int *K,double *bg,int *dim,int *S,double *cvr,int *Z
             ,int *E,int *verb,double *res);
void bilinear(double *src,int *dim,int *S,int *verb,double *dst);
void optimalDatatypeDouble(double *x,int *n,int *res);
void focalCorrel(double *x,int *dim,double *bg,double *sz,double *S,double *A
            ,double *cvr,int *fz,int *E,int *verb,double *res);
void readBilLineDouble(char **fname,int *dim,int *index,int *nindex,int *dtype
                       ,int *byteorder,double *res);
int focalSobelG(double *x,int *dim,double *bg
          ,double *_size,double *_sigma,double *_alpha
          ,double *cvr,int *_fillNA,int *_saveMargin,int *verb,double *res);
int progressBar(int cur,int max,char *text);
void writeBilBandInteger(char **fname,int *value,int *dim,int *index
                        ,int *nindex,int *dtype,int *byteorder);
void readBilBandInteger(char **fname,int *dim,int *index,int *nindex,int *dtype
                       ,int *byteorder,int *res);
void conTest(int *adr,int *res);
void readBilLineInteger(char **fname,int *dim,int *index,int *nindex,int *dtype
                       ,int *byteorder,int *res);
void optimalDatatypeInt(int *x,int *n,int *res);
void ffocal4(double *x,int *dim,double *bg,int *sz,int *fill
                ,double *cvr,int *knd,int *verb,double *res);
void readBilBandDouble(char **fname,int *dim,int *index,int *nindex,int *dtype
                       ,int *byteorder,double *res);
void internalMargin(double *x,int *dim,int *indr,int *indc);
void focalLoG(double *x,int *dim,double *bg,double *sz,double *S,double *A
             ,double *cvr,int *fz,int *E,int *verb,double *res);
void areaIncrement(double *x,int *dim,double *res,double *out);
void rasterize(double *img,int *dim,double *bbox
              ,double *crdx,double *crdy,double *value,double *nodata
              ,int *len,int *_kind);
void readBsqBandInteger(char **fname,int *dim,int *index,int *nindex,int *dtype
                       ,int *byteorder,int *res);
void focalMeanWithNA(double *x,int *dim,double *W,double *cvr,int *Z,int *verbose
                  ,double *res);
void readBsqLineDouble(char **fname,int *dim,int *index,int *nindex,int *dtype
                       ,int *byteorder,double *res);
void readBilLineDouble2(char **fname,int *dim,int *index,int *nindex,int *dtype
                       ,int *byteorder,double *res);
void focalHires(double *x,int *dim,double *bg,double *sz,double *S,double *A
               ,double *cvr,int *fz,int *E,int *verb,double *res);
void reclassify(double *src,int *_n,double *na,double *class,int *_nclass,int *dst);
void aggregate(double *x,int *dim,int *S,double *cvr,int *verb,double *res);
void readBilLineInteger2(char **fname,int *dim,int *index,int *nindex,int *dtype
                       ,int *byteorder,int *res);
void resampl4(double *obj1,double *bg,int *dim1,int *dim2,double *lim1
             ,double *lim2,double *C,double *A,int *V,double *obj2);
void focalMedian(double *x,double *bg,int *dim,int *S,int *F,int *E,double *cvr
            ,int *verbose,double *res);
void timefilt4(double *x,int *dim,int *win,double *cover,double *res);
void makemap4(double *x,double *_bg,int *dim,double *_cover,double *weight
             ,int *_sum,double *res);
void focalGaussian(double *x,int *dim,double *bg,double *sz,double *S,double *A
             ,double *cvr,int *fz,int *E,int *verb,double *res);
void readBsqBandDouble(char **fname,int *dim,int *index,int *nindex,int *dtype
                       ,int *byteorder,double *res);
int focalSobel(double *x,int *dim,double *bg
          ,double *_size,double *_sigma,double *_alpha
          ,double *cvr,int *_fillNA,int *_saveMargin,int *verb,double *res);
void writeBilBandDouble(char **fname,double *value,int *dim,int *index
                        ,int *nindex,int *dtype,int *byteorder);
void focalLaplacian(double *x,int *dim,double *bg,double *sz,double *S,double *A
               ,double *cvr,int *fz,int *E,int *verb,double *res);
void scatterplot(int *x,int *y,int *n,int *nbreakX,int *nbreakY
                ,int *histX,int *histY,int *hist2d);
