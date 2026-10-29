# Utility: Extract Transparent KAWE Icon and Assemble Horizontal Brand Assets
# Data: /home/kawe/Pictures/KAWE.jpeg
# Output: www/kawe-icon-transparent.png, www/kawe_logo_horizontal_transparent.png

from PIL import Image
import numpy as np
import os

def extract_logo_assets():
    input_path = "/home/kawe/Pictures/KAWE.jpeg"
    output_dir = "/home/kawe/GitHub/multilevel-meta-analysis/www"
    os.makedirs(output_dir, exist_ok=True)
    
    # 1. Open original master image
    img = Image.open(input_path).convert("RGBA")
    data = np.array(img)
    
    # 2. Convert white background to transparent (threshold > 240)
    r, g, b, a = data[:, :, 0], data[:, :, 1], data[:, :, 2], data[:, :, 3]
    white_mask = (r > 240) & (g > 240) & (b > 240)
    data[white_mask] = [255, 255, 255, 0]
    transparent_img = Image.fromarray(data)
    
    w, h = transparent_img.size
    
    # 3. Find optimal vertical split between curve bottom and KAWE top (~55% - 58%)
    window_start = int(h * 0.48)
    window_end = int(h * 0.62)
    row_alphas = [np.sum(data[y, :, 3] > 0) for y in range(h)]
    min_y = window_start + np.argmin(row_alphas[window_start:window_end])
    split_y = min_y
    
    icon_crop = transparent_img.crop((0, 0, w, split_y))
    icon_crop = icon_crop.crop(icon_crop.getbbox())
    
    text_crop = transparent_img.crop((0, split_y, w, h))
    text_crop = text_crop.crop(text_crop.getbbox())
    
    # Save transparent icon
    icon_path = os.path.join(output_dir, "kawe-icon-transparent.png")
    icon_crop.save(icon_path)
    print(f"Icon saved: {icon_path} (size: {icon_crop.size})")
    
    # 4. Assemble horizontal logo (Icon left, Text right, vertically centered)
    gap = 60
    padding = 40
    new_w = icon_crop.width + gap + text_crop.width + (padding * 2)
    new_h = max(icon_crop.height, text_crop.height) + (padding * 2)
    
    horizontal_logo = Image.new("RGBA", (new_w, new_h), (255, 255, 255, 0))
    icon_y = (new_h - icon_crop.height) // 2
    horizontal_logo.paste(icon_crop, (padding, icon_y), icon_crop)
    
    text_x = padding + icon_crop.width + gap
    text_y = (new_h - text_crop.height) // 2
    horizontal_logo.paste(text_crop, (text_x, text_y), text_crop)
    
    horiz_path = os.path.join(output_dir, "kawe_logo_horizontal_transparent.png")
    horizontal_logo.save(horiz_path)
    print(f"Horizontal logo saved: {horiz_path} (size: {horizontal_logo.size})")

if __name__ == "__main__":
    extract_logo_assets()
