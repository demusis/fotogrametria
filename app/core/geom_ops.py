import numpy as np
import cv2
from scipy.optimize import minimize
from PySide6.QtGui import QImage, QPixmap

def cv2_to_qpixmap(cv_img):
    """Converte array (H, W, 3) float64 [0,1] ou uint8 [0,255] para QPixmap."""
    if cv_img.dtype == np.float64 or cv_img.dtype == np.float32:
        cv_img = (np.clip(cv_img, 0, 1) * 255).astype(np.uint8)
    h, w, c = cv_img.shape
    bytes_per_line = c * w
    qimg = QImage(cv_img.data, w, h, bytes_per_line, QImage.Format.Format_RGB888)
    return QPixmap.fromImage(qimg)

def qpixmap_to_cv2(pixmap: QPixmap):
    qimg = pixmap.toImage().convertToFormat(QImage.Format.Format_RGB888)
    w, h = qimg.width(), qimg.height()
    ptr = qimg.bits()
    arr = np.array(ptr).reshape((h, w, 3))
    return arr.astype(np.float64) / 255.0

def distort_coords(pts_x, pts_y, k_params, center_x, center_y, norm_factor):
    k1, k2, p1, p2, k3 = k_params
    cx = (pts_x - center_x) / norm_factor
    cy = (pts_y - center_y) / norm_factor
    r2 = cx**2 + cy**2
    r4 = r2**2
    r6 = r2**3
    
    f = 1 + k1 * r2 + k2 * r4 + k3 * r6
    dx = 2 * p1 * cx * cy + p2 * (r2 + 2 * cx**2)
    dy = p1 * (r2 + 2 * cy**2) + 2 * p2 * cx * cy
    
    dist_x = (cx * f + dx) * norm_factor + center_x
    dist_y = (cy * f + dy) * norm_factor + center_y
    return dist_x, dist_y

def optimize_lens_distortion(polylines, center_x, center_y, norm_factor, initial_k=(0.0, 0.0, 0.0, 0.0, 0.0)):
    def error_function(k_params):
        total_error = 0.0
        for poly in polylines:
            if len(poly) < 2:
                continue
            xs, ys = [p[0] for p in poly], [p[1] for p in poly]
            start_x, start_y = xs[0], ys[0]
            end_x, end_y = xs[-1], ys[-1]
            vx, vy = end_x - start_x, end_y - start_y
            v_sq_len = vx**2 + vy**2
            if v_sq_len == 0:
                continue
            
            w_xs = np.array(xs) - start_x
            w_ys = np.array(ys) - start_y
            
            t = (w_xs * vx + w_ys * vy) / v_sq_len
            proj_x = start_x + t * vx
            proj_y = start_y + t * vy
            
            dist_x, dist_y = distort_coords(proj_x, proj_y, k_params, center_x, center_y, norm_factor)
            error = np.sum((np.array(xs) - dist_x)**2 + (np.array(ys) - dist_y)**2)
            total_error += error
        return total_error

    res = minimize(error_function, initial_k, method='L-BFGS-B', 
                   bounds=[(-3.0, 3.0), (-1.5, 1.5), (-0.5, 0.5), (-0.5, 0.5), (-1.0, 1.0)])
    return res.x, res.success
