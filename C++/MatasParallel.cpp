/*
 * MatasParallel.cpp
 *
 *  Created on: Oct 12, 2012
 *      Author: pedrormjunior
 */

#include <iostream>
#include <stack>

#include <cv.h>
#include <highgui.h>

#define PARALLEL_MATAS

extern "C" {
#include <math.h>
#include <sys/time.h>
#ifdef PARALLEL_MATAS
#include <pthread.h>
#endif  // PARALLEL_MATAS
}

using namespace cv;
using namespace std;

Point **ptree;                  // global point-tree
Mat img;

void PTree() {
  ptree = new Point*[img.rows];
  ptree[0] = new Point[img.rows * img.cols];
  for(int i = 1; i < img.rows; ++i) {
    ptree[i] = ptree[i-1] + img.cols;
  }
}

void FinishPTree() {
  delete[] ptree[0];
  delete[] ptree;
}

Point par(const Point &p) {
  return ptree[p.y][p.x];
}

void par(const Point &a, const Point &b) {
  ptree[a.y][a.x] = b;
}

int f(const Point &p) {
  int aux = (int)img.at<uchar>(p.y, p.x);
  return aux;
}

void *build1DPar(void *pline) {
  int line = *((int*)pline);

  stack<Point> stk;

  Point r(0, line);
  for(int j = 1; j < img.cols; ++j) {
    Point p(j, line);
    if(f(r) < f(p)) {
      stk.push(r);
      r = p;
    }
    else if(f(r) == f(p)) {
      par(p, r);
    }
    else while(true) {
        Point q;
        if(! stk.empty()) {
          q = stk.top();
        }
        if(stk.empty() || f(q) < f(p)) {
          par(r, p);
          r = p;
          break;
        }
        else if(f(q) == f(p)) {
          par(r, q);
          par(p, q);
          r = q;
          stk.pop();
          break;
        }
        else {
          par(r, q);
          r = q;
          stk.pop();
        }
      }
  }
  while(! stk.empty()) {
    par(r, stk.top());
    r = stk.top();
    stk.pop();
  }
  Point und(-1, -1);
  par(r, und);

  return NULL;
}

void build1D() {
#ifdef PARALLEL_MATAS
  pthread_t thread_ID[img.rows];
  int arguments[img.rows];
#endif  // PARALLEL_MATAS
  for(int i = 0; i < img.rows; ++i) {
#ifdef PARALLEL_MATAS
    arguments[i] = i;
    pthread_create(thread_ID+i, NULL, build1DPar, arguments+i);
#else
    build1DPar(&i);
#endif  // PARALLEL_MATAS
  }

#ifdef PARALLEL_MATAS
  for(int i = 0; i < img.rows; ++i) {
    pthread_join(thread_ID[i], NULL);
  }
#endif  // PARALLEL_MATAS
}

Point levroot(const Point &x) {
  if(par(x).x == -1 || par(x).y == -1) {
    return x;
  }
  else if(f(x) == f(par(x))) {
    par(x, levroot(par(x)));
    return par(x);
  }
  else {
    return x;
  }
}

void connect(Point x, Point y) {
  x = levroot(x);
  y = levroot(y);
  if(f(y) > f(x)) {
    Point swap = x;
    x = y;
    y = swap;
  }
  while(x != y) {
    if(par(x).x == -1) {
      par(x, y);
      x = y;
    }
    else {
      Point z = levroot(par(x));
      if(f(z) > f(y)) {
        x = z;
      }
      else {
        par(x, y);
        x = y;
        y = z;
      }
    }
  }
}

void *merge(void *pborder) {
  int border = *((int*)pborder);

  for(int j = 0; j < img.cols; ++j) {
    Point x(j, border - 1);
    Point y(j, border    );
    connect(x, y);
  }

  return NULL;
}

vector<vector<int> > linesToMerge() {
  vector<vector<int> > lines;
  for(int i = 1; i < img.rows; ++i) {
    vector<int> line;
    int v = (int)pow(2., i-1);
    if(! (v < img.rows)) break;
    for(; v < img.rows; v += (int)pow(2., i)) {
      line.push_back(v);
    }
    lines.push_back(line);
  }

  return lines;
}

void mergeAll() {
  vector<vector<int> > lines = linesToMerge();
#ifdef PARALLEL_MATAS
  pthread_t thread_ID[lines[0].size()];
#endif  // PARALLEL_MATAS
  for(unsigned int i = 0; i < lines.size(); ++i) {
    for(unsigned int j = 0; j < lines[i].size(); ++j) {
#ifdef PARALLEL_MATAS
      pthread_create(thread_ID+j, NULL, merge, &lines[i][j]);
#else
      merge(&lines[i][j]);
#endif  // PARALLEL_MATAS
    }

#ifdef PARALLEL_MATAS
    for(unsigned int j = 0; j < lines[i].size(); ++j) {
      pthread_join(thread_ID[j], NULL);
    }
#endif  // PARALLEL_MATAS
  }
}

void matas() {
  build1D();
  mergeAll();
}

void print() {
  for(int i = 0; i < img.rows; ++i) {
    for(int j = 0; j < img.cols; ++j) {
      cout << "[" << ptree[i][j].y << ", " << ptree[i][j].x << "] ";
    }
    cout << endl;
  }
}

void printLinesToMerge(vector<vector<int> > lines) {
  for(unsigned int i = 0; i < lines.size(); ++i) {
    for(unsigned int j = 0; j < lines[i].size(); ++j) {
      cout << lines[i][j] << " ";
    }
    cout << endl;
  }
}

int main(int argc, char** argv) {
  img = imread(argv[1], 0);
  if(argc != 2 || !img.data) {
    cout << "No image data" << endl;
    return -1;
  }

  PTree();

  struct timeval start, end;
  gettimeofday(&start, NULL);
  matas();
  gettimeofday(&end, NULL);
  double exec_time = ((double)end.tv_sec - start.tv_sec) +
    ((double)end.tv_usec - start.tv_usec)/1000000.0;

  cout << exec_time << endl;

  //	print();

  FinishPTree();

  cerr << "Finished " << argv[1] << "!!!" << endl;

  return 0;
}
